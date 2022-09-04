package components

import (
	"fmt"
	"go2async/internal/globalArguments"
	"go2async/pkg/variable"
	"strconv"
	"strings"
)

const scopePrefix = "SC_"
const defaultScopeEntityName = "Scope"

var scopeEntitesTracker = make(map[string]bool)

type Scope struct {
	Nr       int
	archName string

	Block *Block

	Params     map[string]*variable.VariableInfo
	Variables  map[string]*variable.VariableInfo
	ReturnVars []*variable.VariableInfo

	OutReg *Reg

	In  *HandshakeChannel
	Out *HandshakeChannel
}

var scopeNr = 0

func NewScope(name string, block *Block, params map[string]*variable.VariableInfo, returnVars []*variable.VariableInfo) *Scope {
	nr := scopeNr
	if name == "" {
		scopeNr++
		name = strings.ToLower(scopePrefix + strconv.Itoa(nr))
	}
	s := &Scope{
		Nr:       nr,
		archName: name,
		Block:    block,
		In: &HandshakeChannel{
			Out: false,
		},
		Params:     params,
		Variables:  make(map[string]*variable.VariableInfo),
		ReturnVars: returnVars,
	}

	entryIn := &HandshakeChannel{
		Req:  "in_req",
		Ack:  "in_ack",
		Data: "in_data",
		Out:  true,
	}

	s.Block.In = entryIn

	rs := 0
	for _, s := range s.ReturnVars {
		rs += s.Size
	}

	s.OutReg = NewReg(s.Block.OutputSize, false, "0")

	s.Block.Out.Connect(s.OutReg.In)

	s.Out = s.OutReg.Out

	return s
}

func (s *Scope) InChannel() *HandshakeChannel {
	return s.In
}

func (s *Scope) OutChannel() *HandshakeChannel {
	return s.Out
}

func (s *Scope) entityName() string {
	extIntfLen := len(s.Block.ExternalInterfaces)
	if extIntfLen == 0 {
		return defaultScopeEntityName
	}
	return defaultScopeEntityName + "_" + strconv.Itoa(extIntfLen)
}

func (s *Scope) Entity() string {
	if _, ok := scopeEntitesTracker[s.entityName()]; ok {
		return ``
	}
	blockEntitesTracker[s.entityName()] = ""

	if *globalArguments.Debug {
		fmt.Printf("Generating unique block entity '%s'\n", s.entityName())
	}

	externalInterfacesStr := ``
	externalIntferacesGenericsStr := ``
	semiColon := ``
	if len(s.Block.ExternalInterfaces) > 0 {
		semiColon = `;`
	}

	i := 0
	for _, extIntf := range s.Block.ExternalInterfaces {
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

		if i != len(s.Block.ExternalInterfaces)-1 {
			externalIntferacesGenericsStr += ";\n"
			externalInterfacesStr += ";\n"
		}
		i++
	}

	ret := `
	LIBRARY IEEE;
	USE IEEE.STD_LOGIC_1164.ALL;
	USE ieee.std_logic_unsigned.ALL;
	USE ieee.numeric_std.ALL;
	USE work.click_element_library_constants.ALL;
	
	ENTITY ` + s.entityName() + ` IS
	  GENERIC (
		DATA_WIDTH : NATURAL := 8;
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
		` + externalInterfacesStr + `
	  );
	END ` + s.entityName() + `;`

	return ret
}

func (s *Scope) Component() string {
	name := scopePrefix + strconv.Itoa(s.Nr)

	externalInterfacesStr := ``
	externalIntferacesGenericsStr := ``
	comma := ``
	if len(s.Block.ExternalInterfaces) > 0 {
		comma = `,`
	}

	i := 0
	for _, extIntf := range s.Block.ExternalInterfaces {
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

		if i != len(s.Block.ExternalInterfaces)-1 {
			externalInterfacesStr += ",\n"
			externalIntferacesGenericsStr += ",\n"
		}
		i++
	}

	return name + `: entity work.` + s.entityName() + `(` + s.archName + `)
  generic map(
    DATA_WIDTH => ` + s.archName + `_DATA_WIDTH,
	OUT_DATA_WIDTH => ` + s.archName + `_OUT_DATA_WIDTH,
	IN_DATA_WIDTH => ` + s.archName + `_IN_DATA_WIDTH` + comma + `
	` + externalIntferacesGenericsStr + `
  )
  port map (
   rst => rst,
   -- Input channel
   in_ack => ` + s.In.Ack + `,
   in_req => ` + s.In.Req + `,
   in_data => ` + s.In.Data + `,
   -- Output channel
   out_req => ` + s.Out.Req + `,
   out_data => ` + s.Out.Data + `,
   out_ack => ` + s.Out.Ack + comma + `
   -- External interfaces
   ` + externalInterfacesStr + `
  );
  `
}

func (s *Scope) signalDefs() string {

	ret := SignalsString(s.Block.Out)
	ret += SignalsString(s.OutReg.Out)

	return ret
}

func (s *Scope) Architecture() string {
	// TODO: add inner components
	ret := `architecture ` + s.archName + ` of ` + s.entityName() + ` is
	`

	ret += s.signalDefs()
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += "out_req <= " + s.OutChannel().Req + "; \n"
	ret += s.OutChannel().Ack + " <= out_ack; \n"
	ret += "out_data <= "
	for _, v := range s.ReturnVars {
		if v.Len == 1 {
			idx := getIndex(v.Index)
			ret += s.OutReg.OutChannel().Data + "(" + strconv.Itoa(v.Position+v.Size*(idx+1)) + " -1 downto " + strconv.Itoa(v.Position+v.Size*idx) + ") & "
		} else {
			for idx := v.Len - 1; idx >= 0; idx-- {
				ret += s.OutReg.OutChannel().Data + "(" + strconv.Itoa(v.Position+v.Size*(idx+1)) + " -1 downto " + strconv.Itoa(v.Position+v.Size*idx) + ") & "
			}
		}
	}
	ret = strings.TrimSuffix(ret, " & ")

	ret += ";\n"

	ret += "\n"

	ret += s.Block.ComponentStr()
	ret += "\n"
	ret += s.OutReg.ComponentStr()
	ret += "\n"

	ret += `end ` + s.archName + `;
	`
	return ret
}

func (s *Scope) ArchName() string {
	return s.archName
}
