package components

import (
	"errors"
	"fmt"
	"go2async/internal/globalArguments"
	"go2async/pkg/variable"
	"strconv"
	"strings"
)

const blockPrefix = "B_"
const defaultBlockEntityName = "BlockC"

type regBodyPair struct {
	Reg *Reg
	Bc  BodyComponentType
}

type FuncInterface struct {
	Name       string
	Parameters *ScopedVariables
	Results    *ScopedVariables

	In  *HandshakeChannel
	Out *HandshakeChannel
}

func NewFuncInterface(name string) *FuncInterface {
	return &FuncInterface{
		Name:       name,
		Parameters: NewScopedVariables(nil),
		Results:    NewScopedVariables(nil),

		// External interface: in is output and out is input
		In: &HandshakeChannel{
			Req:  name + "_in_req",
			Ack:  name + "_in_ack",
			Data: name + "_in_data",
			Out:  true,
		},
		Out: &HandshakeChannel{
			Req:  name + "_out_req",
			Ack:  name + "_out_ack",
			Data: name + "_out_data",
			Out:  false,
		},
	}
}

func (fi *FuncInterface) GetOutChannel() *HandshakeChannel {
	return fi.Out
}

func (fi *FuncInterface) GetInChannel() *HandshakeChannel {
	return fi.In
}

func (fi *FuncInterface) Copy() *FuncInterface {
	return &FuncInterface{
		Name:       fi.Name,
		Parameters: fi.Parameters,
		Results:    fi.Results,

		// External interface: in is output and out is input
		In: &HandshakeChannel{
			Req:  fi.Name + "_in_req",
			Ack:  fi.Name + "_in_ack",
			Data: fi.Name + "_in_data",
			Out:  true,
		},
		Out: &HandshakeChannel{
			Req:  fi.Name + "_out_req",
			Ack:  fi.Name + "_out_ack",
			Data: fi.Name + "_out_data",
			Out:  false,
		},
	}
}

type Block struct {
	BodyComponent
	Nr int

	TopLevel       bool
	RegBlockPairs  []*regBodyPair
	BodyComponents []BodyComponentType

	scopedVariables *ScopedVariables

	ExternalInterfaces map[string]*FuncInterface

	OutputSize int
}

var blockNr = 0

func NewBlock(toplevel bool, parent *Block) *Block {
	nr := blockNr
	blockNr++

	name := strings.ToLower(blockPrefix + strconv.Itoa(nr))
	b := &Block{
		BodyComponent: BodyComponent{
			archName: archPrefix + name,
			In: &HandshakeChannel{
				Req:  "in_req",
				Ack:  "in_ack",
				Data: "in_data",
				Out:  false,
			},
			Out: &HandshakeChannel{
				Req:  "bl_" + strconv.Itoa(nr) + "_out_req",
				Ack:  "bl_" + strconv.Itoa(nr) + "_out_ack",
				Data: "bl_" + strconv.Itoa(nr) + "_data_out",
				Out:  true,
			},
			parent:       parent,
			variableSize: parent.GetCurrentVariableSize(),
		},

		Nr:       nr,
		TopLevel: toplevel,

		scopedVariables: NewScopedVariables(parent),

		ExternalInterfaces: make(map[string]*FuncInterface),
	}

	return b
}

func NewParamDummyBlock(params map[string]*variable.VariableInfo) *Block {
	ret := &Block{
		ExternalInterfaces: make(map[string]*FuncInterface),
	}

	ret.parent = nil
	ret.scopedVariables = &ScopedVariables{
		variables: params,
	}

	ret.scopedVariables.size = 0
	for _, v := range params {
		ret.scopedVariables.size += v.Len * v.Size
	}

	return ret
}

func (b *Block) AddComponent(bodyComponent BodyComponentType) {
	b.Out = bodyComponent.OutChannel()

	if b.TopLevel {
		newreg := NewReg(bodyComponent.GetVariableSize(), false, "0")
		newreg.Out.Connect(bodyComponent.InChannel())

		if len(b.RegBlockPairs) == 0 {
			*newreg.InChannel() = *b.In
		} else {
			b.Out = bodyComponent.OutChannel()
			b.RegBlockPairs[len(b.RegBlockPairs)-1].Bc.OutChannel().Connect(newreg.InChannel())
		}

		b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{newreg, bodyComponent})
	} else {
		b.Out = bodyComponent.OutChannel()

		if len(b.RegBlockPairs) == 0 {
			entryIn := &HandshakeChannel{
				Req:  "in_req",
				Ack:  "in_ack",
				Data: "in_data",
				Out:  true,
			}
			*bodyComponent.InChannel() = *entryIn
		} else {
			b.Out = bodyComponent.OutChannel()
			b.RegBlockPairs[len(b.RegBlockPairs)-1].Bc.OutChannel().Connect(bodyComponent.InChannel())
		}

		b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{Bc: bodyComponent})
	}
}

func (b *Block) EntityName() string {
	return defaultBlockEntityName + "_" + strconv.Itoa(len(b.ExternalInterfaces))
}

func (b *Block) ComponentStr() string {
	name := blockPrefix + strconv.Itoa(b.Nr)

	dataInWidth := strconv.Itoa(b.GetVariableSize())
	dataOutWidth := dataInWidth
	if len(b.RegBlockPairs) > 0 {
		dataOutWidth = strconv.Itoa(b.RegBlockPairs[len(b.RegBlockPairs)-1].Bc.GetVariableSize())
	}

	externalInterfacesStr := ``
	externalIntferacesGenericsStr := ``
	comma := ``
	if len(b.ExternalInterfaces) > 0 {
		comma = `,`
	}

	i := 0
	for _, extIntf := range b.ExternalInterfaces {
		externalIntferacesGenericsStr += extIntf.Name + `_DATA_IN_WIDTH => ` + extIntf.Name + "_DATA_IN_WIDTH,\n"
		externalIntferacesGenericsStr += extIntf.Name + `_DATA_OUT_WIDTH => ` + extIntf.Name + `_DATA_OUT_WIDTH`

		externalInterfacesStr += `-- Interface for ` + extIntf.Name
		externalInterfacesStr += `
		-- Input channel
		` + extIntf.Name + `_in_data  => ` + extIntf.Name + `_in_data,
		` + extIntf.Name + `_in_req => ` + extIntf.Name + `_in_req,
		` + extIntf.Name + `_in_ack => ` + extIntf.Name + `_in_ack,
		-- Output channel
		` + extIntf.Name + `_out_data => ` + extIntf.Name + `_out_data,
		` + extIntf.Name + `_out_req => ` + extIntf.Name + `_out_req,
		` + extIntf.Name + `_out_ack => ` + extIntf.Name + `_out_ack`

		if i != len(b.ExternalInterfaces)-1 {
			externalInterfacesStr += ",\n"
			externalIntferacesGenericsStr += ",\n"
		}
		i++
	}

	return name + `: entity work.` + b.EntityName() + `(` + b.archName + `)
  generic map(
   DATA_IN_WIDTH => ` + dataInWidth + `,
   DATA_OUT_WIDTH => ` + dataOutWidth + comma + `
   ` + externalIntferacesGenericsStr + `
  )
  port map (
	rst => rst,
	-- Input channel
	in_ack => ` + b.In.Ack + `,
	in_req => ` + b.In.Req + `,
	in_data => std_logic_vector(resize(unsigned(` + b.In.Data + `), ` + strconv.Itoa(b.GetVariableSize()) + `)),
	-- Output channel
	out_req => ` + b.Out.Req + `,
	out_data => ` + b.Out.Data + `,
	out_ack => ` + b.Out.Ack + comma + `
	-- External interfaces
	` + externalInterfacesStr + `
  );
  `
	//out_data => ` + b.Out.Data + `(` + b.Out.Data + `'length - 1 downto ` + b.Out.Data + `'length - ` + strconv.Itoa(*b.GetVariablesSize()) + `),
}

func (b *Block) signalDefs() string {
	if len(b.RegBlockPairs) == 0 {
		return ""
	}

	ret := ""

	for _, c := range b.RegBlockPairs {
		if c.Reg != nil {
			ret += SignalsString(c.Reg.OutChannel())
		}
		ret += SignalsString(c.Bc.OutChannel())
	}

	/* for _, extIntf := range b.externalInterfaces {
		ret += SignalsString(extIntf.GetInChannel())
	} */

	return ret
}

func (b *Block) ioChannels() string {
	ret := ""
	if len(b.RegBlockPairs) == 0 {
		ret += "out_req <= in_req; \n"
		ret += "in_ack <= out_ack; \n"
		ret += "out_data <= in_data; \n"
	} else {
		ret += "out_req <= " + b.OutChannel().Req + "; \n"
		ret += b.OutChannel().Ack + " <= out_ack; \n"
		ret += "out_data <= std_logic_vector(resize(unsigned(" + b.OutChannel().Data + "), out_data'length)); \n"
	}

	/* for _, extIntf := range b.externalInterfaces {
		ret += extIntf.Name + "_out_req" + "; \n"
		ret += extIntf.Name + "" + "; \n"
		ret += extIntf.Name + "" + "; \n"
	} */

	return ret
}

func (b *Block) componentsString() string {
	ret := ""
	for _, c := range b.RegBlockPairs {
		if c.Reg != nil {
			ret += c.Reg.ComponentStr()
		}
		ret += c.Bc.ComponentStr()
	}

	return ret
}

func (b *Block) Entity() string {
	if *globalArguments.Debug {
		fmt.Printf("Generating unique block entity '%s'\n", b.EntityName())
	}

	externalInterfacesStr := ``
	externalIntferacesGenericsStr := ``
	semiColon := ``
	if len(b.ExternalInterfaces) > 0 {
		semiColon = `;`
	}

	i := 0
	for _, extIntf := range b.ExternalInterfaces {
		externalIntferacesGenericsStr += extIntf.Name + "_DATA_IN_WIDTH : NATURAL := 8;\n"
		externalIntferacesGenericsStr += extIntf.Name + `_DATA_OUT_WIDTH : NATURAL := 8`

		externalInterfacesStr += `-- Interface for ` + extIntf.Name
		externalInterfacesStr += `
		-- Input channel
		` + extIntf.Name + `_in_data : OUT STD_LOGIC_VECTOR(` + extIntf.Name + `_DATA_IN_WIDTH - 1 DOWNTO 0);
		` + extIntf.Name + `_in_req : OUT STD_LOGIC;
		` + extIntf.Name + `_in_ack : IN STD_LOGIC;
		-- Output channel
		` + extIntf.Name + `_out_data : IN STD_LOGIC_VECTOR(` + extIntf.Name + `_DATA_OUT_WIDTH - 1 DOWNTO 0);
		` + extIntf.Name + `_out_req : IN STD_LOGIC;
		` + extIntf.Name + `_out_ack : OUT STD_LOGIC`

		if i != len(b.ExternalInterfaces)-1 {
			externalIntferacesGenericsStr += ";\n"
			externalInterfacesStr += ";\n"
		}
		i++
	}

	ret := `LIBRARY IEEE;
	USE IEEE.STD_LOGIC_1164.ALL;
	USE ieee.std_logic_unsigned.ALL;
	USE ieee.numeric_std.ALL;
	USE work.click_element_library_constants.ALL;
	
	ENTITY ` + b.EntityName() + ` IS
	  GENERIC (
		DATA_IN_WIDTH : NATURAL := 8;
		DATA_OUT_WIDTH : NATURAL := 8` + semiColon + `
		` + externalIntferacesGenericsStr + `
	  );
	  PORT (
		rst : IN STD_LOGIC;
		-- Input channel
		in_data : IN STD_LOGIC_VECTOR(DATA_IN_WIDTH - 1 DOWNTO 0);
		in_req : IN STD_LOGIC;
		in_ack : OUT STD_LOGIC;
		-- Output channel
		out_data : OUT STD_LOGIC_VECTOR(DATA_OUT_WIDTH - 1 DOWNTO 0);
		out_req : OUT STD_LOGIC;
		out_ack : IN STD_LOGIC` + semiColon + `

		-- External interfaces
		` + externalInterfacesStr +
		`
	  );
	END ` + b.EntityName() + `;`

	return ret
}

func (b *Block) Architecture() string {
	ret := `architecture ` + b.archName + ` of ` + b.EntityName() + ` is
	`

	ret += b.signalDefs()
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += b.ioChannels()

	ret += "\n"

	ret += b.componentsString()

	ret += `end ` + b.archName + `;
	`
	return ret
}

func (b *Block) ScopedVariables() *ScopedVariables {
	return b.scopedVariables
}

func (b *Block) GetCurrentVariableSize() int {
	return b.scopedVariables.size
}

func (b *Block) NewVariable(name string, typ string, len int) (*variable.VariableInfo, error) {
	return b.ScopedVariables().AddVariable(name, typ, len)
}

func (b *Block) GetVariable(name string) (*variable.VariableInfo, error) {
	v, err := b.ScopedVariables().GetVariableInfo(name)
	if err != nil {
		if b.Parent() == nil {
			return nil, err
		}

		return b.Parent().GetVariable(name)
	} else {
		return v, nil
	}
}

func (b *Block) AddFunctionInterface(f *FuncInterface) error {
	if _, ok := b.ExternalInterfaces[f.Name]; ok {
		return errors.New("Functionpointer already decalred")
	}

	b.ExternalInterfaces[f.Name] = f

	return nil
}

func (b *Block) GetAndAssignFunctionInterface(fname string) (*FuncInterface, error) {
	if f, ok := b.ExternalInterfaces[fname]; ok {
		return f, nil
	}

	if *globalArguments.Debug {
		fmt.Printf("Function '%s' not found at block %s - searching parent\n", fname, b.archName)
	}

	if b.parent != nil {
		f, err := b.parent.GetAndAssignFunctionInterface(fname)
		if err == nil {
			// parent-stack had the function defined: add the interface to block
			fiCopy := f.Copy()
			b.ExternalInterfaces[f.Name] = fiCopy

			if *globalArguments.Debug {
				fmt.Printf("Found function '%s' on parent stack and registerd function '%s' at block %s\n", f.Name, f.Name, b.archName)
			}

			return f, nil
		}
	}

	return nil, errors.New("No function defined for " + fname)
}
