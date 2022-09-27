package components

import (
	"errors"
	"go2async/pkg/variable"
	"strconv"
	"strings"
)

const funcblockprefix = "FB_"
const defaultFuncBlockEntityName = "funcBlock"

type FuncBlock struct {
	BodyComponent

	Nr int

	paramsResults *variable.FuncInterface

	externalInterface *variable.VariableInfo
}

var fbNr = 0

func NewFuncBlock(paramsResults *variable.FuncInterface, fi variable.VariableDef, parent *Block) (*FuncBlock, error) {
	if fi.FuncIntf() == nil {
		return nil, errors.New("invalid function variable '" + fi.Name() + "'")
	}

	nr := bebNr
	bebNr++

	name := strings.ToLower(binexprblockprefix + strconv.Itoa(nr))

	f, err := parent.GetAndAssignFunctionInterface(fi.Name())
	if err != nil {
		return nil, err
	}

	ret := &FuncBlock{
		BodyComponent: BodyComponent{
			archName: archPrefix + name,

			In: &HandshakeChannel{
				Out: false,
			},

			Out: &HandshakeChannel{
				Req:       name + "_o_req",
				Ack:       name + "_o_ack",
				Data:      name + "_data",
				Out:       true,
				DataWidth: parent.GetCurrentVariableSize(),
			},

			parent:       parent,
			variableSize: parent.GetCurrentVariableSize(),
		},

		Nr: nr,

		paramsResults: paramsResults,

		externalInterface: f.Copy(),
	}

	return ret, nil
}

func (fb *FuncBlock) InChannel() *HandshakeChannel {
	return fb.In
}

func (fb *FuncBlock) OutChannel() *HandshakeChannel {
	return fb.Out
}

func (fb *FuncBlock) EntityName() string {
	return defaultFuncBlockEntityName + "_" + fb.externalInterface.Name()
}

func (fb *FuncBlock) Entity() string {
	return `LIBRARY IEEE;
	USE IEEE.STD_LOGIC_1164.ALL;
	USE ieee.std_logic_unsigned.ALL;
	USE ieee.numeric_std.ALL;
	USE work.click_element_library_constants.ALL;
	
	ENTITY ` + fb.EntityName() + ` IS
	  GENERIC (
		DATA_WIDTH : NATURAL := 8;
		` + fb.externalInterface.Name() + `_DATA_IN_WIDTH : NATURAL := 8;
		` + fb.externalInterface.Name() + `_DATA_OUT_WIDTH : NATURAL := 8
	  );
	  PORT (
		-- Input channel
		in_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
		in_req : IN STD_LOGIC;
		in_ack : OUT STD_LOGIC;
		-- Output channel
		out_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
		out_req : OUT STD_LOGIC;
		out_ack : IN STD_LOGIC;

		-- External interfaces
		` + fb.externalInterface.Name() + `_in_data : OUT STD_LOGIC_VECTOR(` + fb.externalInterface.Name() + `_DATA_IN_WIDTH - 1 DOWNTO 0);
		` + fb.externalInterface.Name() + `_in_req : OUT STD_LOGIC;
		` + fb.externalInterface.Name() + `_in_ack : IN STD_LOGIC;
		-- Output channel
		` + fb.externalInterface.Name() + `_out_data : IN STD_LOGIC_VECTOR(` + fb.externalInterface.Name() + `_DATA_OUT_WIDTH - 1 DOWNTO 0);
		` + fb.externalInterface.Name() + `_out_req : IN STD_LOGIC;
		` + fb.externalInterface.Name() + `_out_ack : OUT STD_LOGIC
	  );
	END ` + fb.EntityName() + `;`
}

func (fb *FuncBlock) ComponentStr() string {
	name := binexprblockprefix + strconv.Itoa(fb.Nr)

	return name + `: entity work.` + fb.EntityName() + `(` + fb.archName + `)
	generic map(
	  DATA_WIDTH => ` + strconv.Itoa(fb.GetVariableSize()) + `,
	  ` + fb.externalInterface.Name() + `_DATA_IN_WIDTH => ` + fb.externalInterface.Name() + `_DATA_IN_WIDTH,
	  ` + fb.externalInterface.Name() + `_DATA_OUT_WIDTH => ` + fb.externalInterface.Name() + `_DATA_OUT_WIDTH
	)
	port map (
		-- Input channel
		in_req  => ` + fb.In.Req + `,
		in_ack  => ` + fb.In.Ack + `, 
		in_data => std_logic_vector(resize(unsigned(` + fb.In.Data + `), ` + strconv.Itoa(fb.GetVariableSize()) + `)),
		-- Output channel
		out_req => ` + fb.Out.Req + `,
		out_ack => ` + fb.Out.Ack + `,
		out_data  => ` + fb.Out.Data + `,

		--External Interface
		-- Input channel
		` + fb.externalInterface.Name() + `_in_data  => ` + fb.externalInterface.Name() + `_in_data,
		` + fb.externalInterface.Name() + `_in_req => ` + fb.externalInterface.Name() + `_in_req,
		` + fb.externalInterface.Name() + `_in_ack => ` + fb.externalInterface.Name() + `_in_ack,
		-- Output channel
		` + fb.externalInterface.Name() + `_out_data => ` + fb.externalInterface.Name() + `_out_data,
		` + fb.externalInterface.Name() + `_out_req => ` + fb.externalInterface.Name() + `_out_req,
		` + fb.externalInterface.Name() + `_out_ack => ` + fb.externalInterface.Name() + `_out_ack
	);
	`
}

func (fb *FuncBlock) getAliases() string {
	ret := ""

	for i, paramVar := range fb.paramsResults.Parameters.VariableList {
		is := strconv.Itoa(i)
		if paramVar.IndexIdent_ == nil {
			idx := getIndex(paramVar.Index_)
			totalSize := paramVar.Size_ * paramVar.Len_
			ret += "alias x_" + is + " : std_logic_vector(" + strconv.Itoa(totalSize) + " - 1 downto 0)  is in_data( " + strconv.Itoa(paramVar.Position_+totalSize*(idx+1)) + " - 1 downto " + strconv.Itoa(paramVar.Position_+totalSize*idx) + ");\n"
		} else {
			ret += "signal x_" + is + " : std_logic_vector(" + strconv.Itoa(paramVar.Size_) + "- 1 downto 0);\n"
			ret += "constant baseX_" + is + " : integer := " + strconv.Itoa(paramVar.Position_) + ";\n"
			ret += "alias offsetX_" + is + " : std_logic_vector(" + strconv.Itoa(paramVar.IndexIdent_.Size_) + " - 1 downto 0)  is in_data( " + strconv.Itoa(paramVar.IndexIdent_.Position_+paramVar.IndexIdent_.Size_) + " -1 downto " + strconv.Itoa(paramVar.IndexIdent_.Position_) + ");\n"
		}
	}

	for i, resVar := range fb.paramsResults.Results.VariableList {
		is := strconv.Itoa(i)
		if resVar.IndexIdent_ == nil {
			idx := getIndex(resVar.Index_)
			totalSize := resVar.Size_ * resVar.Len_
			ret += "alias result_" + is + " : std_logic_vector(" + strconv.Itoa(totalSize) + " - 1 downto 0)  is out_data( " + strconv.Itoa(resVar.Position_+totalSize*(idx+1)) + " - 1 downto " + strconv.Itoa(resVar.Position_+totalSize*idx) + ");\n"
		} else {
			ret += "signal result_" + is + " : std_logic_vector(" + strconv.Itoa(resVar.Size_) + " - 1 downto 0);\n"
			ret += "constant baseR_" + is + " : integer := " + strconv.Itoa(resVar.Position_) + ";\n"
			ret += "alias offsetR_" + is + " : std_logic_vector(" + strconv.Itoa(resVar.IndexIdent_.Size_) + " - 1 downto 0)  is in_data( " + strconv.Itoa(resVar.IndexIdent_.Position_+resVar.IndexIdent_.Size_) + " -1 downto " + strconv.Itoa(resVar.IndexIdent_.Position_) + ");\n"
		}
	}

	return ret
}

func (fb *FuncBlock) getProcess() string {

	variables := ""
	for i, resVar := range fb.paramsResults.Results.VariableList {
		if resVar.IndexIdent_ == nil {
			is := strconv.Itoa(i)
			variables += "variable offset_" + is + ": integer range 0 to out_data'length;\n"
		}
	}

	processStart := `calc: process(all)
	` + variables + `
	begin
	out_data <= in_data; 
	`

	arrayParamMappings := ""
	resultMap := ""
	delay := " after ADDER_DELAY"
	compute := ""

	for i, paramVar := range fb.paramsResults.Parameters.VariableList {
		if paramVar.IndexIdent_ != nil {
			is := strconv.Itoa(i)
			arrayParamMappings += "x_" + is + " <= in_data(baseX_" + is + " + (to_integer(unsigned(offsetX_" + is + ")) + 1) * x_" + is + "'length  - 1 downto baseX_" + is + " + to_integer(unsigned(offsetX_" + is + ")) * x_" + is + "'length);\n"
		}
	}

	resultSignalOffset := 0
	for i, resVar := range fb.paramsResults.Results.VariableList {
		is := strconv.Itoa(i)
		if resVar.IndexIdent_ != nil {
			resultMap += "offset_" + is + " := baseR_" + is + " + to_integer(unsigned(offsetR_" + is + ") * result_" + is + "'length);\n"
			resultMap += "out_data(offset_" + is + " + result_" + is + "'length -1 downto offset_" + is + ") <= result_" + is + delay + ";\n"
		}

		totalSize := resVar.Size_ * resVar.Len_
		compute += "result_" + is + " <=  " + fb.externalInterface.Name() + "_out_data( " + strconv.Itoa(resultSignalOffset+totalSize) + " - 1 downto " + strconv.Itoa(resultSignalOffset) + ")" + delay + ";\n"
		resultSignalOffset += totalSize
	}

	return processStart + arrayParamMappings + resultMap + compute + `
	end process;`
}

func (fb *FuncBlock) Architecture() string {
	// TODO: analyze delays

	externalIntfInput := ""

	if len(fb.paramsResults.Parameters.VariableList) > 0 {
		externalIntfInput += fb.externalInterface.Name() + "_in_data <= "
		for i, _ := range fb.paramsResults.Parameters.VariableList {
			is := strconv.Itoa(i)
			externalIntfInput += "x_" + is

			if i != len(fb.paramsResults.Parameters.VariableList)-1 {
				externalIntfInput += " & "
			}
		}
		externalIntfInput += ";"
	}

	return `architecture ` + fb.archName + ` of ` + fb.EntityName() + ` is
	` + fb.getAliases() + `
  begin
    ` + fb.externalInterface.Name() + `_in_req <= in_req;
    in_ack <= ` + fb.externalInterface.Name() + `_in_ack;
    ` + fb.externalInterface.Name() + `_out_ack <= out_ack;

	` + externalIntfInput + `
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => 0  -- Delay  size
      )
      port map (
        i => ` + fb.externalInterface.Name() + `_out_req,
        o => out_req
	  );
	  
	  ` + fb.getProcess() + `

  end ` + fb.archName + `;
  `
}
