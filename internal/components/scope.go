package components

import (
	infoPrinter "go2async/internal/infoPrinter"
	"go2async/internal/variable"
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
	ReturnVars []*variable.VariableInfo

	OutReg *Reg

	In  *HandshakeChannel
	Out *HandshakeChannel
}

var scopeNr = 0

func NewScope(name string, block *Block) *Scope {
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
	}

	/* entryIn := &HandshakeChannel{
		Req:  "in_req",
		Ack:  "in_ack",
		Data: "in_data",
		Out:  true,
	}

	s.Block.In = entryIn
	*/
	rs := 0
	for _, s := range s.ReturnVars {
		rs += s.Size_
	}

	s.OutReg = NewReg(s.Block, s.Block.OutputVariables(), false, "0")
	/*
		s.Block.Out.Connect(s.OutReg.In)

		s.Out = s.OutReg.Out */

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
	infoPrinter.DebugPrintf("Generating unique scope entity '%s'\n", s.EntityName())

	externalInterfacesStr := ``
	externalIntferacesGenericsStr := ``
	semiColon := ``
	if len(s.Block.ExternalInterfaces) > 0 {
		semiColon = `;`
	}

	i := 0
	for name, _ := range s.Block.ExternalInterfaces {
		externalIntferacesGenericsStr += name + "_IN_DATA_WIDTH : NATURAL := 8;\n"
		externalIntferacesGenericsStr += name + `_OUT_DATA_WIDTH : NATURAL := 8`

		externalInterfacesStr += `-- Interface for ` + name
		externalInterfacesStr += `
		-- Input channel
		` + name + `_in_data : OUT STD_LOGIC_VECTOR(` + name + `_IN_DATA_WIDTH - 1 DOWNTO 0);
		` + name + `_in_req : OUT STD_LOGIC;
		` + name + `_in_ack : IN STD_LOGIC;
		-- Output channel
		` + name + `_out_data : IN STD_LOGIC_VECTOR(` + name + `_OUT_DATA_WIDTH - 1 DOWNTO 0);
		` + name + `_out_req : IN STD_LOGIC;
		` + name + `_out_ack : OUT STD_LOGIC`

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

func (s *Scope) Name() string {
	return scopePrefix + strconv.Itoa(s.Nr)
}

func (s *Scope) ComponentStr() string {
	externalInterfacesStr := ``
	externalIntferacesGenericsStr := ``
	comma := ``
	if len(s.Block.ExternalInterfaces) > 0 {
		comma = `,`
	}

	i := 0
	for fName, extIntf := range s.Block.ExternalInterfaces {
		externalIntferacesGenericsStr += fName + `_IN_DATA_WIDTH => ` + fName + "_IN_DATA_WIDTH,\n"
		externalIntferacesGenericsStr += fName + `_OUT_DATA_WIDTH => ` + fName + `_OUT_DATA_WIDTH`

		externalInterfacesStr += `-- Interface for ` + fName
		externalInterfacesStr += `
		-- Input channel
		` + fName + `_in_req => ` + extIntf.In.GetReqSignalName() + `,
		` + fName + `_in_ack => ` + extIntf.In.GetAckSignalName() + `,
		` + fName + `_in_data  => ` + extIntf.InData.GetDataSignalName() + `,
		-- Output channel
		` + fName + `_out_req => ` + extIntf.Out.GetReqSignalName() + `,
		` + fName + `_out_ack => ` + extIntf.Out.GetReqSignalName() + `,
		` + fName + `_out_data => ` + extIntf.OutData.GetDataSignalName() + `
		`

		if i != len(s.Block.ExternalInterfaces)-1 {
			externalInterfacesStr += ",\n"
			externalIntferacesGenericsStr += ",\n"
		}
		i++
	}

	return s.Name() + `: entity work.` + s.EntityName() + `(` + s.archName + `)
  generic map(
    DATA_WIDTH => ` + s.Name() + `_DATA_WIDTH,
	OUT_DATA_WIDTH => ` + s.Name() + `_OUT_DATA_WIDTH,
	IN_DATA_WIDTH => ` + s.Name() + `_IN_DATA_WIDTH` + comma + `
	` + externalIntferacesGenericsStr + `
  )
  port map (
	rst => rst,
	-- Input channel
	in_req => ` + s.Name() + `_in_req,
	in_ack => ` + s.Name() + `_in_ack,
	in_data => ` + s.Name() + `_in_data,
	-- Output channel
	out_req => ` + s.Name() + `_out_req,
	out_ack => ` + s.Name() + `_out_ack,
	out_data => ` + s.Name() + `_out_data ` + comma + `
	-- External interfaces
	` + externalInterfacesStr + `
  `
}

func (s *Scope) signalDefs() string {
	/* 	ret := s.Block.Out.SignalsString()
	   	ret += s.OutReg.Out.SignalsString() */
	ret := ""

	// Block
	ret += "signal " + s.Block.Name() + "_in_req : std_logic;"
	ret += "signal " + s.Block.Name() + "_in_ack : std_logic;"
	ret += "signal " + s.Block.Name() + "_in_data : std_logic_vector(" + strconv.Itoa(s.Block.InputVariables().Size) + " - 1 downto 0);"
	ret += "\n"

	ret += "signal " + s.Block.Name() + "_out_req : std_logic;"
	ret += "signal " + s.Block.Name() + "_out_ack : std_logic;"
	ret += "signal " + s.Block.Name() + "_out_data : std_logic_vector(" + strconv.Itoa(s.Block.OutputVariables().Size) + " - 1 downto 0);"
	ret += "\n"

	for _, extIntf := range s.Block.ExternalInterfaces {
		ret += extIntf.In.SignalDefs()
		ret += "\n"
		ret += extIntf.Out.SignalDefs()
		ret += "\n"
		ret += extIntf.InData.SignalDefs()
		ret += extIntf.OutData.SignalDefs()
		ret += "\n"
	}

	// OutReg
	ret += "signal " + s.OutReg.Name() + "_in_req : std_logic;"
	ret += "signal " + s.OutReg.Name() + "_in_ack : std_logic;"
	ret += "signal " + s.OutReg.Name() + "_in_data : std_logic_vector(" + strconv.Itoa(s.Block.OutputVariables().Size) + " - 1 downto 0);"
	ret += "\n"

	ret += "signal " + s.OutReg.Name() + "_out_req : std_logic;"
	ret += "signal " + s.OutReg.Name() + "_out_ack : std_logic;"
	ret += "signal " + s.OutReg.Name() + "_out_data : std_logic_vector(" + strconv.Itoa(s.Block.OutputVariables().Size) + " - 1 downto 0);"
	ret += "\n"

	return ret
}

func (s *Scope) signalAssignments() string {
	ret := ""

	// Scope Inputs to Block
	ret += s.Block.Name() + "_in_req <= in_req;"
	ret += "\n"
	ret += "in_ack <= " + s.Block.Name() + "_in_ack;"
	ret += "\n"
	ret += s.Block.Name() + "_in_data <= in_data;"
	ret += "\n"

	// Block to Reg
	ret += s.OutReg.Name() + "_in_req <= " + s.Block.Name() + "_out_req;"
	ret += "\n"
	ret += s.Block.Name() + "_out_ack <= " + s.OutReg.Name() + "_in_ack;"
	ret += "\n"
	ret += s.OutReg.Name() + "_in_data <= " + s.Block.Name() + "_out_data;"
	ret += "\n"

	// Reg to Scope Ouput
	ret += "out_req <= " + s.OutReg.Name() + "_out_req;"
	ret += "\n"
	ret += s.OutReg.Name() + "_out_ack <= out_ack;"
	ret += "\n"
	ret += "out_data <= " + s.OutReg.Name() + "_out_data;"
	ret += "\n"

	for name, extIntf := range s.Block.ExternalInterfaces {
		ret += name + "_in_req <= " + extIntf.In.GetReqSignalName() + ";"
		ret += "\n"
		ret += name + "_out_ack <= " + extIntf.Out.GetAckSignalName() + ";"
		ret += "\n"
		ret += extIntf.In.GetAckSignalName() + " <= " + name + "_in_ack;"
		ret += "\n"
		ret += extIntf.Out.GetReqSignalName() + " <= " + name + "_out_req;"
		ret += "\n"
		ret += name + "_in_data <= " + extIntf.InData.GetDataSignalName() + ";"
		ret += "\n"
		ret += extIntf.OutData.GetDataSignalName() + " <= " + name + "_out_data;"
		ret += "\n"

		ret += "\n"
	}

	return ret
}

func (s *Scope) Architecture() string {
	ret := `architecture ` + s.archName + ` of ` + s.EntityName() + ` is
	`

	ret += s.signalDefs()
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += s.signalAssignments()

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
