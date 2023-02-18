package components

import (
	"errors"
	"fmt"
	"go2async/internal/globalArguments"
	"go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"strconv"
	"strings"
)

const binexprblockprefix = "BEP_"

// TODO: check delays for more operations

var SupportedOperations map[string]string = map[string]string{"+": "+", "-": "-", "<<": "sll", ">>": "srl", "|": "or", "&": "and", "NOP": "", "=": ""}
var operationDelays map[string]string = map[string]string{"+": "ADD_DELAY", "-": "ADD_DELAY", "<<": "ADD_DELAY", ">>": "ADD_DELAY", "|": "ADD_DELAY", "&": "ADD_DELAY", "NOP": "ADD_DELAY", "=": "ADD_DELAY"}

type BinExprBlock struct {
	BodyComponent

	Operation string

	Oi *OperandInfo

	opDescription string // debug

	addedInput int
}

type OperandInfo struct {
	R, X, Y *variable.VariableInfo
}

var bebNr = 0

func NewBinExprBlock(op string, oi *OperandInfo, parent BlockType) (*BinExprBlock, error) {
	nr := bebNr
	bebNr++

	name := binexprblockprefix + strconv.Itoa(nr)

	bep := &BinExprBlock{
		BodyComponent: BodyComponent{
			name: strings.ToLower(binexprblockprefix + strconv.Itoa(nr)),

			number:   nr,
			archName: archPrefix + name,

			parentBlock: parent,

			inputVariables:  variable.NewScopedVariables(),
			outputVariables: variable.NewScopedVariables(),

			predecessors: map[string]BodyComponentType{},
			successors:   map[string]BodyComponentType{},
		},

		Operation: op,
		Oi:        oi,

		addedInput: 0,
	}

	inputChannel := NewDefaultInputHandshakeChannel(bep)
	bep.In = append(bep.In, inputChannel)

	outputChannel := NewDefaultOutputHandshakeChannel(bep)
	bep.Out = append(bep.Out, outputChannel)

	bep.InData = append(bep.InData, NewDataChannel(bep, variable.NewScopedVariables(), bep.Name()+"_x", false))
	bep.InData = append(bep.InData, NewDataChannel(bep, variable.NewScopedVariables(), bep.Name()+"_y", false))

	bep.OutData = append(bep.OutData, NewDataChannel(bep, bep.OutputVariables(), bep.Name()+"_result", true))

	if *globalArguments.Debug {
		opDescription := fmt.Sprintf("binExprBlock '%s': %s [size %d, len %d, index %s; const %s] = %s [size %d, len %d, index %s; const %s] ",
			bep.Name(), oi.R.Name_, oi.R.Size_, oi.R.Len_, oi.R.Index_, oi.R.Const_, oi.X.Name_, oi.X.Size_, oi.X.Len_, oi.X.Index_, oi.X.Const_)

		opDescription += op

		if oi.Y != nil {
			opDescription += fmt.Sprintf(" %s [size %d, len %d, index %s; const %s]", oi.Y.Name_, oi.Y.Size_, oi.Y.Len_, oi.Y.Index_, oi.Y.Const_)
		}

		infoPrinter.DebugPrintfln("[%s]: Creating %s - parent: ", parent.ArchName(), opDescription)

		opDescription += fmt.Sprintf("\n")

		bep.opDescription = opDescription
	}

	if op != "NOP" {
		// get sources of X, Y
		if _, err := getOperandOwnersAndSetNewOwner(bep, parent, oi); err != nil {
			return nil, err
		}

		infoPrinter.DebugPrintfln("[%s] finished owner assignments of [%s]", parent.Name(), bep.Name())
	}

	return bep, nil
}

func getInputSources(bt BodyComponentType, parent BlockType, oi *OperandInfo, resultType string, holdOffConnections bool) error {
	// get sources of X, Y
	{
		if oi.X == nil {
			constVar, err := variable.MakeConst("0", resultType)
			if err != nil {
				return err
			}

			oi.X = constVar
		}

		if oi.X.DefinedOnly_ {
			panic("using defined only variable")
		}

		if oi.X.Const_ != "" {
			oi.X.Typ_ = resultType
			infoPrinter.DebugPrintfln("[%s]: X Input '%s' is const '%s' with type %s inferred from result", bt.Name(), oi.X.Name_, oi.X.Const_, resultType)
		}

		addedVar, err := bt.AddInputVariable(oi.X)
		if err != nil {
			return err
		}
		oi.X = addedVar

		infoPrinter.DebugPrintfln("[%s]: X Input '%s' type %s", bt.Name(), oi.X.Name_, resultType)

		ownX, ok := parent.GetVariableOwnerMap()[oi.X.Name_]
		if !ok {
			// Shouldn't happen
			return errors.New("No owner for X operator")
		}

		latestOwner := ownX.ownerList.lastest
		ownX.ownerList.AddSuccessorToLatest(bt)

		bt.AddPredecessor(latestOwner)
		latestOwner.AddSuccessor(bt)

		//bt.InChannels()[0].ConnectHandshake(latestOwner.OutChannels()[0])

		if !holdOffConnections {
			bt.ConnectHandshake(latestOwner)

			// X is first inDataChannel
			// Assume components only have 1 outputDataChannel here
			//bt.InDataChannels()[0].ConnectData(latestOwner.OutDataChannels()[0])
			bt.ConnectDataPos(latestOwner, 0, 0)
		}

		infoPrinter.DebugPrintfln("[%s]: Previous component of X input '%s' is '%s'", bt.Name(), oi.X.Name_, latestOwner.Name())
	}

	{
		if oi.Y == nil {
			constVar, err := variable.MakeConst("0", resultType)
			if err != nil {
				return err
			}

			oi.Y = constVar
		}

		if oi.Y.DefinedOnly_ {
			panic("using defined only variable")
		}

		if oi.Y.Const_ != "" {
			oi.Y.Typ_ = resultType
			infoPrinter.DebugPrintfln("[%s]: Y Input '%s' is const '%s' with type %s inferred from result", bt.Name(), oi.Y.Name_, oi.Y.Const_, resultType)
		}

		addedVar, err := bt.AddInputVariable(oi.Y)
		if err != nil {
			return err
		}
		oi.Y = addedVar

		infoPrinter.DebugPrintfln("[%s]: Y Input '%s' type %s", bt.Name(), oi.Y.Name_, resultType)

		ownY, ok := parent.GetVariableOwnerMap()[oi.Y.Name_]
		if !ok {
			// Shouldn't happen
			return errors.New("No owner for Y operator")
		}

		latestOwner := ownY.ownerList.lastest
		ownY.ownerList.AddSuccessorToLatest(bt)

		bt.AddPredecessor(latestOwner)
		latestOwner.AddSuccessor(bt)

		if !holdOffConnections {
			//bt.InChannels()[0].ConnectHandshake(latestOwner.OutChannels()[0])
			bt.ConnectHandshake(latestOwner)

			// Y is second inDataChannel
			// Assume components only have 1 outputDataChannel here
			// bt.InDataChannels()[1].ConnectData(latestOwner.OutDataChannels()[0])
			bt.ConnectDataPos(latestOwner, 1, 0)
		}

		infoPrinter.DebugPrintfln("[%s]: Previous component of Y input '%s' is '%s'", bt.Name(), oi.Y.Name_, latestOwner.Name())
	}

	return nil
}

func getOperandOwnersAndSetNewOwner(bt BodyComponentType, parent BlockType, oi *OperandInfo) (BodyComponentType, error) {
	resultType := ""

	// Do this step first, to fetch the resultType
	if oi.R != nil {
		oi.R.DefinedOnly_ = false

		if _, err := bt.AddOutputVariable(oi.R); err != nil {
			return nil, err
		}

		resultType = oi.R.Typ_

		infoPrinter.DebugPrintfln("[%s] Result variable '%s' type '%s' added to outputs", bt.Name(), oi.R.Name(), oi.R.Typ())
	} else {
		panic("Missing result var")
	}

	err := getInputSources(bt, parent, oi, resultType, false)
	if err != nil {
		return nil, err
	}

	// Set new owner of result variable after getting previous owners! - R is not nil here!
	if own, ok := parent.GetVariableOwnerMap()[oi.R.Name()]; ok {
		own.vi = oi.R
		own.ownerList.AddOwner(bt)
	} else {
		infoPrinter.DebugPrintfln("Adding variable '%s' to ownermap of '%s'", oi.R.Name_, parent.Name())

		parent.GetVariableOwnerMap()[oi.R.Name()] = &variableOwner{
			ownerList: NewOwnerList(bt),
			vi:        oi.R,
		}
	}

	infoPrinter.DebugPrintfln("New owner of variable '%s' is new binExprBlock '%s'", oi.R.Name_, bt.Name())

	return bt, nil
}

func (bc *BinExprBlock) AddInputVariable(vtd *variable.VariableInfo) (*variable.VariableInfo, error) {
	// Allow both inputs to be the same
	vi, err := bc.InputVariables().AddVariable(vtd)
	if err != nil {
		vi, err = bc.InputVariables().GetVariableInfo(vtd.Name())
		if err != nil {
			return nil, err
		}
	}

	// Add inputs to their respecting inputChannels
	if bc.addedInput >= 2 {
		panic("cannot add more than 2 inputs to binExpr")
	} else if bc.addedInput == 1 {
		bc.InDataChannels()[1].variables.AddVariable(vtd)
	} else {
		bc.InDataChannels()[0].variables.AddVariable(vtd)

	}
	bc.addedInput++

	if !bc.isBlock {
		// Check owner map
		parent := bc.parentBlock
		if _, ok := parent.GetVariableOwnerMap()[vi.Name()]; !ok {

			parent.GetVariableOwnerMap()[vi.Name()] = &variableOwner{
				ownerList: NewOwnerList(parent),
				vi:        vtd,
			}

			infoPrinter.DebugPrintfln("[%s]: No owner for variable '%s' found. Making parent %s owner", bc.archName, vi.Name(), parent.Name())
		}
	}

	return vi, err
}

func (bep *BinExprBlock) GetXTotalSize() int {
	x, err := bep.InputVariables().GetVariableInfoAt(0)
	if err != nil {
		panic(bep.Name() + " invalid x")
	}

	return x.TotalSize()
}
func (bep *BinExprBlock) GetYTotalSize() int {
	y, err := bep.InputVariables().GetVariableInfoAt(1)
	if err != nil {
		panic(bep.Name() + " invalid y")
	}

	return y.TotalSize()
}
func (bep *BinExprBlock) GetRTotalSize() int {
	r, err := bep.OutputVariables().GetVariableInfoAt(0)
	if err != nil {
		panic(bep.Name() + " invalid r")
	}

	return r.TotalSize()
}

func (bep *BinExprBlock) Name() string {
	return bep.name
}

func (bep *BinExprBlock) ComponentStr() string {
	return bep.Name() + `: entity work.` + bep.EntityName() + `(` + bep.ArchName() + `)
	port map (
	  -- Input channel
	  in_req  => ` + bep.In[0].GetReqSignalName() + `,
	  in_ack  => ` + bep.In[0].GetAckSignalName() + `, 
	  x => ` + bep.InData[0].GetDataSignalName() + `,
	  y => ` + bep.InData[1].GetDataSignalName() + `,
	  -- Output channel
	  out_req => ` + bep.Out[0].GetReqSignalName() + `,
	  out_ack => ` + bep.Out[0].GetAckSignalName() + `,
	  result  => ` + bep.OutData[0].GetDataSignalName() + `
	);
	`
}

func getIndex(idxStd string) int {
	idx, _ := strconv.Atoi(idxStd)
	return idx
}

func (bep *BinExprBlock) getAliases() string {
	ret := ""
	if bep.Oi.X.IndexIdent_ == nil {
		if bep.Oi.X.Const_ == "" {
			idx := getIndex(bep.Oi.X.Index_)
			totalSize := bep.Oi.X.Size_ * bep.Oi.X.Len_
			if idx > 0 {
				totalSize = bep.Oi.X.Size_
			}
			ret += "alias x : std_logic_vector(" + strconv.Itoa(totalSize) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.X.Position_+totalSize*(idx+1)) + " - 1 downto " + strconv.Itoa(bep.Oi.X.Position_+totalSize*idx) + ");\n"
		}
	} else {
		ret += "signal x : std_logic_vector(" + strconv.Itoa(bep.Oi.X.Size_) + "- 1 downto 0);\n"
		ret += "constant baseX      : integer := " + strconv.Itoa(bep.Oi.X.Position_) + ";\n"
		ret += "alias offsetX      : std_logic_vector(" + strconv.Itoa(bep.Oi.X.IndexIdent_.Size_) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.X.IndexIdent_.Position_+bep.Oi.X.IndexIdent_.Size_) + " -1 downto " + strconv.Itoa(bep.Oi.X.IndexIdent_.Position_) + ");\n"
	}

	if bep.Oi.Y != nil && bep.Oi.Y.IndexIdent_ == nil {
		if bep.Oi.Y.Const_ == "" {
			idx := getIndex(bep.Oi.Y.Index_)
			totalSize := bep.Oi.Y.Size_ * bep.Oi.Y.Len_
			if idx > 0 {
				totalSize = bep.Oi.Y.Size_
			}
			ret += "alias y      : std_logic_vector(" + strconv.Itoa(totalSize) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.Y.Position_+totalSize*(idx+1)) + " - 1 downto " + strconv.Itoa(bep.Oi.Y.Position_+totalSize*idx) + ");\n"
		}
	} else if bep.Oi.Y != nil && bep.Oi.Y.IndexIdent_ != nil {
		ret += "signal y  : std_logic_vector(" + strconv.Itoa(bep.Oi.Y.Size_) + "- 1 downto 0);\n"
		ret += "constant baseY      : integer := " + strconv.Itoa(bep.Oi.Y.Position_) + ";\n"
		ret += "alias offsetY      : std_logic_vector(" + strconv.Itoa(bep.Oi.Y.IndexIdent_.Size_) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.Y.IndexIdent_.Position_+bep.Oi.Y.IndexIdent_.Size_) + " -1 downto " + strconv.Itoa(bep.Oi.Y.IndexIdent_.Position_) + ");\n"
	}

	if bep.Oi.R.IndexIdent_ == nil {
		idx := getIndex(bep.Oi.R.Index_)
		totalSize := bep.Oi.R.Size_ * bep.Oi.R.Len_
		if idx > 0 {
			totalSize = bep.Oi.R.Size_
		}
		ret += "alias result : std_logic_vector(" + strconv.Itoa(totalSize) + " - 1 downto 0)  is out_data( " + strconv.Itoa(bep.Oi.R.Position_+totalSize*(idx+1)) + " - 1 downto " + strconv.Itoa(bep.Oi.R.Position_+totalSize*idx) + ");\n"
	} else {
		ret += "signal result : std_logic_vector(" + strconv.Itoa(bep.Oi.R.Size_) + " - 1 downto 0);\n"
		ret += "constant baseR      : integer := " + strconv.Itoa(bep.Oi.R.Position_) + ";\n"
		ret += "alias offsetR      : std_logic_vector(" + strconv.Itoa(bep.Oi.R.IndexIdent_.Size_) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.R.IndexIdent_.Position_+bep.Oi.R.IndexIdent_.Size_) + " -1 downto " + strconv.Itoa(bep.Oi.R.IndexIdent_.Position_) + ");\n"
	}

	return ret
}

func (bep *BinExprBlock) getCalcProcess() string {
	x := ""
	y := ""
	delay := " after ADDER_DELAY"

	x = "unsigned(x)"
	if bep.Oi.X.Const_ != "" {
		x = "to_unsigned(" + bep.Oi.X.Const_ + ", result'length)"
	}

	if bep.Oi.Y != nil {
		y = "unsigned(y)"
		if bep.Oi.Y.Const_ != "" {
			y = "to_unsigned(" + bep.Oi.Y.Const_ + ", result'length)"
		}

		if bep.Operation == "<<" || bep.Operation == ">>" {
			y = "to_integer(" + y + ")"
		}
	}

	if bep.Operation == "NOP" || bep.Operation == "=" || bep.Oi.Y == nil {
		y = ""
		delay = ""
	}

	processStart := `calc: process(all)
	variable offset: integer range 0 to result'length;
	begin
	result <= std_logic_vector(to_unsigned(0, result'length)); 
	`

	xcalc := ""
	ycalc := ""
	compute := ""
	resultMap := ""

	if bep.Operation != "NOP" {
		/* if bep.Oi.X.IndexIdent_ != nil {
			x = "unsigned(x)"
			xcalc = "x <= in_data(baseX + (to_integer(unsigned(offsetX)) + 1) * x'length - 1 downto baseX + to_integer(unsigned(offsetX)) * x'length);\n"
		}

		if bep.Oi.Y != nil && bep.Oi.Y.IndexIdent_ != nil {
			y = "unsigned(y)"
			ycalc = "y <= in_data(baseY + (to_integer(unsigned(offsetY)) + 1) * y'length - 1 downto baseY + to_integer(unsigned(offsetY)) * y'length);\n"

		} */

		compute = "result <= std_logic_vector(resize(" + x + " " + SupportedOperations[bep.Operation] + " " + y + ", result'length)) " + delay + ";\n"

		/* if bep.Oi.R.IndexIdent_ != nil {
			resultMap = "offset := baseR + to_integer(unsigned(offsetR) * result'length);\n"
			resultMap += "out_data(offset + result'length -1 downto offset) <= result;\n"
		} */
	}

	return processStart + xcalc + ycalc + compute + resultMap + `
	end process;`
}

func (bep *BinExprBlock) Architecture() string {
	// TODO: analyze delays

	//` + bep.getAliases() + `
	return `architecture ` + bep.archName + ` of ` + bep.EntityName() + ` is
	  
	-- ` + bep.opDescription + `

    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, result: signal is "true";
	  
    --attribute keep : boolean;
	--attribute keep of  x, y, result: signal is true;
  begin
    in_ack <= out_ack;
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ` + operationDelays[bep.Operation] + `  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
	  );
	  

	  ` + bep.getCalcProcess() + `
  end ` + bep.archName + `;
  `
}

func (bep *BinExprBlock) EntityName() string {
	return "binExprBlock_" + bep.Name()
}

func (bep *BinExprBlock) Entity() string {
	op1Size := 0
	op2Size := 0
	resSize := 0

	if op1, err := bep.InputVariables().GetVariableInfo(bep.Oi.X.Name()); err == nil {
		op1Size = op1.TotalSize()
	} else {
		panic(bep.Name() + " invalid x")
	}

	if op2, err := bep.InputVariables().GetVariableInfo(bep.Oi.Y.Name()); err == nil {
		op2Size = op2.TotalSize()
	} else {
		panic(bep.Name() + " invalid y")
	}

	if res, err := bep.OutputVariables().GetVariableInfo(bep.Oi.R.Name()); err == nil {
		resSize = res.TotalSize()
	} else {
		panic(bep.Name() + " invalid result")
	}

	return `LIBRARY IEEE;
	USE IEEE.STD_LOGIC_1164.ALL;
	USE ieee.std_logic_unsigned.ALL;
	USE ieee.numeric_std.ALL;
	USE work.click_element_library_constants.ALL;
	
	ENTITY ` + bep.EntityName() + ` IS
	  PORT (-- Input channel
		in_req : IN STD_LOGIC;
		in_ack : OUT STD_LOGIC;
		x : IN STD_LOGIC_VECTOR(` + strconv.Itoa(op1Size) + ` - 1 DOWNTO 0);
		y : IN STD_LOGIC_VECTOR(` + strconv.Itoa(op2Size) + ` - 1 DOWNTO 0);
		-- Output channel
		out_req : OUT STD_LOGIC;
		out_ack : IN STD_LOGIC;
		result : OUT STD_LOGIC_VECTOR(` + strconv.Itoa(resSize) + ` - 1 DOWNTO 0));
	END ` + bep.EntityName() + `;
	`
}

func (bep *BinExprBlock) GetVariableLocation(name string) (string, error) {
	if bep.Oi.R.Name_ != name {
		return "", errors.New("Invalid result name")
	}

	return bep.Name() + "_result", nil
}
