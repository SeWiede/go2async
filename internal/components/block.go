package components

import (
	"errors"
	"fmt"
	"go2async/internal/globalArguments"
	infoprinter "go2async/internal/infoPrinter"
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
type Block struct {
	BodyComponent
	Nr int

	TopLevel       bool
	RegBlockPairs  []*regBodyPair
	BodyComponents []BodyComponentType

	scopedVariables *variable.ScopedVariables

	ExternalInterfaces map[string]*variable.VariableInfo

	OutputSize int
}

func NewScopedVariables(parent *Block) *variable.ScopedVariables {
	parentSize := 0
	if parent != nil {
		parentSize = parent.ScopedVariables().Size
	}

	return &variable.ScopedVariables{
		Variables:    make(map[string]*variable.VariableInfo),
		VariableList: []*variable.VariableInfo{},
		Size:         parentSize,
		ParamPos:     0,
	}
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

		ExternalInterfaces: make(map[string]*variable.VariableInfo),
	}

	return b
}

func NewParamDummyBlock(params map[string]*variable.VariableInfo) *Block {
	ret := &Block{
		ExternalInterfaces: make(map[string]*variable.VariableInfo),
	}

	ret.parent = nil
	ret.scopedVariables = &variable.ScopedVariables{
		Variables: params,
	}

	ret.scopedVariables.Size = 0
	for _, v := range params {
		ret.scopedVariables.Size += v.Len_ * v.Size_
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
	for fName, _ := range b.ExternalInterfaces {
		externalIntferacesGenericsStr += fName + `_IN_DATA_WIDTH => ` + fName + "_IN_DATA_WIDTH,\n"
		externalIntferacesGenericsStr += fName + `_OUT_DATA_WIDTH => ` + fName + `_OUT_DATA_WIDTH`

		externalInterfacesStr += `-- Interface for ` + fName
		externalInterfacesStr += `
		-- Input channel
		` + fName + `_in_data  => ` + fName + `_in_data,
		` + fName + `_in_req => ` + fName + `_in_req,
		` + fName + `_in_ack => ` + fName + `_in_ack,
		-- Output channel
		` + fName + `_out_data => ` + fName + `_out_data,
		` + fName + `_out_req => ` + fName + `_out_req,
		` + fName + `_out_ack => ` + fName + `_out_ack`

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
	for fName, _ := range b.ExternalInterfaces {
		externalIntferacesGenericsStr += fName + "_IN_DATA_WIDTH : NATURAL := 8;\n"
		externalIntferacesGenericsStr += fName + `_OUT_DATA_WIDTH : NATURAL := 8`

		externalInterfacesStr += `-- Interface for ` + fName
		externalInterfacesStr += `
		-- Input channel
		` + fName + `_in_data : OUT STD_LOGIC_VECTOR(` + fName + `_IN_DATA_WIDTH - 1 DOWNTO 0);
		` + fName + `_in_req : OUT STD_LOGIC;
		` + fName + `_in_ack : IN STD_LOGIC;
		-- Output channel
		` + fName + `_out_data : IN STD_LOGIC_VECTOR(` + fName + `_OUT_DATA_WIDTH - 1 DOWNTO 0);
		` + fName + `_out_req : IN STD_LOGIC;
		` + fName + `_out_ack : OUT STD_LOGIC`

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

func (b *Block) ScopedVariables() *variable.ScopedVariables {
	return b.scopedVariables
}

func (b *Block) GetCurrentVariableSize() int {
	return b.scopedVariables.Size
}

func (b *Block) NewVariable(decl *variable.VariableTypeDecl) (*variable.VariableInfo, error) {
	return b.ScopedVariables().AddVariable(decl)
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

func (b *Block) AddFunctionInterface(f *variable.VariableInfo) error {
	if _, ok := b.ExternalInterfaces[f.Name_]; ok {
		return errors.New("Functionpointer already decalred")
	}

	b.ExternalInterfaces[f.Name_] = f.Copy()

	infoprinter.DebugPrintf("added func %s to block %s\n", f.Name_, b.archName)

	return nil
}

func (b *Block) GetAndAssignFunctionInterface(fname string) (*variable.VariableInfo, error) {
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
			b.ExternalInterfaces[f.Name_] = fiCopy

			if *globalArguments.Debug {
				fmt.Printf("Found function '%s' on parent stack and registerd function '%s' at block %s\n", f.Name_, f.Name_, b.archName)
			}

			return fiCopy, nil
		}
	}

	return nil, errors.New("No function defined for " + fname)
}
