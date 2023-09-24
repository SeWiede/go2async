package components

import (
	"errors"
	infoPrinter "go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"strconv"
)

const callBlockPrefix = "CB_"
const defaultCallBlockEntityName = "callBlock"

type CallBlock struct {
	BodyComponent

	Nr int

	paramsResults *variable.FuncInterface

	funcIntf variable.VariableDef
}

var cbNr = 0

func getParams(cb BodyComponentType, parent BlockType, pr *variable.FuncInterface, fi variable.VariableDef) (BodyComponentType, error) {

	// Get inputs
	for i, P := range pr.Parameters.VariableList {
		addedVar, err := cb.AddInputVariable(P)
		if err != nil {
			return nil, err
		}

		P = addedVar

		infoPrinter.DebugPrintfln("[%s]: %d Input '%s' type %s", cb.Name(), i, P.Name_, P.Typ_)

		if err := addVariableToLatestAndConnect(cb, parent, P, false); err != nil {
			return nil, err
		}
	}

	for i, R := range pr.Results.VariableList {
		addedVar, err := cb.AddOutputVariable(R)
		if err != nil {
			return nil, err
		}

		R = addedVar

		rCopy := R.Copy()
		rCopy.IndexIdent_ = nil
		rCopy.Index_ = ""
		// Set new owner of result variable after getting previous owners
		if own, ok := parent.GetVariableOwnerMap()[R.Name()]; ok {
			own.vi = rCopy
			own.ownerList.AddOwner(cb)
		} else {
			infoPrinter.DebugPrintfln("Adding variable '%s' to ownermap of '%s'", R.Name_, parent.Name())

			parent.GetVariableOwnerMap()[R.Name()] = &variableOwner{
				ownerList: NewOwnerList(cb),
				vi:        R,
			}
		}

		infoPrinter.DebugPrintfln("[%s]: %d output '%s' type %s", cb.Name(), i, R.Name_, R.Typ_)
	}

	return cb, nil
}

func NewCallBlock(paramsResults *variable.FuncInterface, fi variable.VariableDef, parent BlockType) (*CallBlock, error) {
	if fi.FuncIntf() == nil {
		return nil, errors.New("invalid function variable '" + fi.Name() + "'")
	}

	nr := cbNr
	cbNr++

	name := callBlockPrefix + strconv.Itoa(nr)

	cb := &CallBlock{
		BodyComponent: BodyComponent{
			name: name,

			number:   nr,
			archName: archPrefix + name,

			parentBlock: parent,

			inputVariables:  variable.NewScopedVariables(),
			outputVariables: variable.NewScopedVariables(),

			predecessors: map[string]BodyComponentType{},
			successors:   map[string]BodyComponentType{},
		},

		paramsResults: paramsResults,

		funcIntf: fi,
	}

	inputChannel := NewDefaultInputHandshakeChannel(cb)
	cb.In = append(cb.In, inputChannel)

	outputChannel := NewDefaultOutputHandshakeChannel(cb)
	cb.Out = append(cb.Out, outputChannel)

	cb.InData = append(cb.InData, NewDefaultInDataChannel(cb, cb.InputVariables()))

	cb.OutData = append(cb.OutData, NewDefaultOutDataChannel(cb, cb.OutputVariables()))

	if _, err := getParams(cb, parent, paramsResults, fi); err != nil {
		return nil, err
	}

	return cb, nil
}

func (cb *CallBlock) EntityName() string {
	return defaultCallBlockEntityName + "_" + cb.funcIntf.Name()
}

func (cb *CallBlock) Entity() string {
	infoPrinter.DebugPrintf("Generating unique callBlock entity '%s'\n", cb.EntityName())

	return `LIBRARY IEEE;
	USE IEEE.STD_LOGIC_1164.ALL;
	USE ieee.std_logic_unsigned.ALL;
	USE ieee.numeric_std.ALL;
	USE work.click_element_library_constants.ALL;
	
	ENTITY ` + cb.EntityName() + ` IS
	  GENERIC (
		DATA_WIDTH : NATURAL := 8;
		` + cb.funcIntf.Name() + `_IN_DATA_WIDTH : NATURAL := 8;
		` + cb.funcIntf.Name() + `_OUT_DATA_WIDTH : NATURAL := 8
	  );
	  PORT (
		rst : IN STD_LOGIC;
		-- Input channel
		in_data : IN STD_LOGIC_VECTOR(` + strconv.Itoa(cb.InputVariables().Size) + ` - 1 DOWNTO 0);
		in_req : IN STD_LOGIC;
		in_ack : OUT STD_LOGIC;
		-- Output channel
		out_data : OUT STD_LOGIC_VECTOR(` + strconv.Itoa(cb.OutputVariables().Size) + ` - 1 DOWNTO 0);
		out_req : OUT STD_LOGIC;
		out_ack : IN STD_LOGIC
	  );
	END ` + cb.EntityName() + `;`
}

func (cb *CallBlock) Name() string {
	return callBlockPrefix + strconv.Itoa(cb.Nr)
}

func (cb *CallBlock) ComponentStr() string {
	return cb.Name() + `: entity work.` + cb.EntityName() + `(` + cb.archName + `)
	port map (
		rst => rst,
		-- Input channel
		in_req  => ` + cb.In[0].GetReqSignalName() + `,
		in_ack  => ` + cb.In[0].GetAckSignalName() + `, 
		in_data => ` + cb.InData[0].GetDataSignalName() + `,
		-- Output channel
		out_req => ` + cb.Out[0].GetReqSignalName() + `,
		out_ack => ` + cb.Out[0].GetReqSignalName() + `,
		out_data  => ` + cb.OutData[0].GetDataSignalName() + `
	);
	`
}

func (cb *CallBlock) getAliases() string {
	ret := ""

	for i, paramVar := range cb.paramsResults.Parameters.VariableList {
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

	for i, resVar := range cb.paramsResults.Results.VariableList {
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

func (cb *CallBlock) getProcess() string {

	variables := ""
	for i, resVar := range cb.paramsResults.Results.VariableList {
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

	for i, paramVar := range cb.paramsResults.Parameters.VariableList {
		if paramVar.IndexIdent_ != nil {
			is := strconv.Itoa(i)
			arrayParamMappings += "x_" + is + " <= in_data(baseX_" + is + " + (to_integer(unsigned(offsetX_" + is + ")) + 1) * x_" + is + "'length  - 1 downto baseX_" + is + " + to_integer(unsigned(offsetX_" + is + ")) * x_" + is + "'length);\n"
		}
	}

	resultSignalOffset := 0
	for i, resVar := range cb.paramsResults.Results.VariableList {
		is := strconv.Itoa(i)
		if resVar.IndexIdent_ != nil {
			resultMap += "offset_" + is + " := baseR_" + is + " + to_integer(unsigned(offsetR_" + is + ") * result_" + is + "'length);\n"
			resultMap += "out_data(offset_" + is + " + result_" + is + "'length -1 downto offset_" + is + ") <= result_" + is + delay + ";\n"
		}

		totalSize := resVar.Size_ * resVar.Len_
		compute += "result_" + is + " <=  scope_out_data( " + strconv.Itoa(resultSignalOffset+totalSize) + " - 1 downto " + strconv.Itoa(resultSignalOffset) + ")" + delay + ";\n"
		resultSignalOffset += totalSize
	}

	return processStart + arrayParamMappings + resultMap + compute + `
	end process;`
}

func (cb *CallBlock) Architecture() string {
	// TODO: analyze delays
	// TODO: scopes with external interfaces?

	// TODO: instantiate scope + wirings

	scopeInput := ""

	if len(cb.paramsResults.Parameters.VariableList) > 0 {
		scopeInput += "scope_in_data <= "
		for i, _ := range cb.paramsResults.Parameters.VariableList {
			is := strconv.Itoa(i)
			scopeInput += "x_" + is

			if i != len(cb.paramsResults.Parameters.VariableList)-1 {
				scopeInput += " & "
			}
		}
		scopeInput += ";"
	}

	return `architecture ` + cb.archName + ` of ` + cb.EntityName() + ` is
	` + cb.getAliases() + `
	signal scope_in_req, scope_out_req, scope_in_ack, scope_out_ack : std_logic;
	signal scope_in_data : std_logic_vector(` + strconv.Itoa(cb.funcIntf.FuncIntf().Results.Size) + ` - 1 downto 0);
	signal scope_out_data : std_logic_vector(` + strconv.Itoa(cb.funcIntf.FuncIntf().Results.Size) + ` - 1 downto 0);
  begin
    scope_in_req <= in_req;
    in_ack <= scope_in_ack;
	
    out_req <= scope_out_req;
    scope_out_ack <= out_ack;

	` + scopeInput + `

	` + cb.funcIntf.Name() + `: entity work.Scope(` + cb.funcIntf.Name() + `)
    generic map(
        DATA_IN_WIDTH => ` + strconv.Itoa(cb.funcIntf.FuncIntf().Parameters.Size) + `,
        DATA_OUT_WIDTH => ` + strconv.Itoa(cb.funcIntf.FuncIntf().Results.Size) + `
    )
    port map (
        rst => rst,
        -- input channel
        in_ack => scope_in_ack,
        in_req => scope_in_req,
        in_data => scope_in_Data,
        -- Output channel
        out_req => scope_out_req,
        out_ack => scope_out_ack,
        out_data => scope_out_data
    );
	 
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => 0  -- Delay  size
      )
      port map (
        i => scope_out_req,
        o => out_req
	  );
	  
	  ` + cb.getProcess() + `

  end ` + cb.archName + `;
  `
}

func (cb *CallBlock) Connect(bc BodyComponentType, x interface{}) {
	panic("not implemented")
}
