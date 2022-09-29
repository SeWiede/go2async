package components

import (
	infoPrinter "go2async/internal/infoPrinter"
	"go2async/pkg/variable"
	"strconv"
	"strings"
)

const scopePrefix = "SC_"
const defaultScopeEntityName = "Scope"

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
		rs += s.Size_
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

func (s *Scope) EntityName() string {
	extIntfLen := len(s.Block.ExternalInterfaces)
	if extIntfLen == 0 {
		return defaultScopeEntityName
	}
	return defaultScopeEntityName + "_" + strconv.Itoa(extIntfLen)
}

func (s *Scope) Entity() string {
	infoPrinter.DebugPrintf("Generating unique block entity '%s'\n", s.EntityName())

	externalInterfacesStr := ``
	externalIntferacesGenericsStr := ``
	semiColon := ``
	if len(s.Block.ExternalInterfaces) > 0 {
		semiColon = `;`
	}

	i := 0
	for _, extIntf := range s.Block.ExternalInterfaces {
		externalIntferacesGenericsStr += extIntf.Name_ + "_IN_DATA_WIDTH : NATURAL := 8;\n"
		externalIntferacesGenericsStr += extIntf.Name_ + `_OUT_DATA_WIDTH : NATURAL := 8`

		externalInterfacesStr += `-- Interface for ` + extIntf.Name_
		externalInterfacesStr += `
		-- Input channel
		` + extIntf.Name_ + `_in_data : OUT STD_LOGIC_VECTOR(` + extIntf.Name_ + `_IN_DATA_WIDTH - 1 DOWNTO 0);
		` + extIntf.Name_ + `_in_req : OUT STD_LOGIC;
		` + extIntf.Name_ + `_in_ack : IN STD_LOGIC;
		-- Output channel
		` + extIntf.Name_ + `_out_data : IN STD_LOGIC_VECTOR(` + extIntf.Name_ + `_OUT_DATA_WIDTH - 1 DOWNTO 0);
		` + extIntf.Name_ + `_out_req : IN STD_LOGIC;
		` + extIntf.Name_ + `_out_ack : OUT STD_LOGIC`

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
	
	ENTITY ` + s.EntityName() + ` IS
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
	END ` + s.EntityName() + `;`

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
		externalIntferacesGenericsStr += extIntf.Name_ + `_IN_DATA_WIDTH => ` + extIntf.Name_ + "_IN_DATA_WIDTH,\n"
		externalIntferacesGenericsStr += extIntf.Name_ + `_OUT_DATA_WIDTH => ` + extIntf.Name_ + `_OUT_DATA_WIDTH`

		externalInterfacesStr += `-- Interface for ` + extIntf.Name_
		externalInterfacesStr += `
		-- Input channel
		` + extIntf.Name_ + `_in_data  => ` + extIntf.Name_ + `_in_data,
		` + extIntf.Name_ + `_in_req => ` + extIntf.Name_ + `_in_req,
		` + extIntf.Name_ + `_in_ack => ` + extIntf.Name_ + `_in_ack,
		-- Output channel
		` + extIntf.Name_ + `_out_data => ` + extIntf.Name_ + `_out_data,
		` + extIntf.Name_ + `_out_req => ` + extIntf.Name_ + `_out_req,
		` + extIntf.Name_ + `_out_ack => ` + extIntf.Name_ + `_out_ack`

		if i != len(s.Block.ExternalInterfaces)-1 {
			externalInterfacesStr += ",\n"
			externalIntferacesGenericsStr += ",\n"
		}
		i++
	}

	return name + `: entity work.` + s.EntityName() + `(` + s.archName + `)
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
	ret := `architecture ` + s.archName + ` of ` + s.EntityName() + ` is
	`

	ret += s.signalDefs()
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += "out_req <= " + s.OutChannel().Req + "; \n"
	ret += s.OutChannel().Ack + " <= out_ack; \n"
	ret += "out_data <= "
	for _, v := range s.ReturnVars {
		if v.Len_ == 1 {
			idx := getIndex(v.Index_)
			ret += s.OutReg.OutChannel().Data + "(" + strconv.Itoa(v.Position_+v.Size_*(idx+1)) + " -1 downto " + strconv.Itoa(v.Position_+v.Size_*idx) + ") & "
		} else {
			for idx := v.Len_ - 1; idx >= 0; idx-- {
				ret += s.OutReg.OutChannel().Data + "(" + strconv.Itoa(v.Position_+v.Size_*(idx+1)) + " -1 downto " + strconv.Itoa(v.Position_+v.Size_*idx) + ") & "
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
