package components

import (
	"go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"strconv"
	"strings"
)

const selectorprefix = "SE_"

// TODO: check delays
var SupportedComperators map[string]string = map[string]string{"==": "=", "!=": "/=", "<": "<", ">": ">", ">=": ">=", "<=": "<="}
var comperatorDelays map[string]string = map[string]string{"==": "ADD_DELAY", "!=": "ADD_DELAY", "<": "ADD_DELAY", ">": "ADD_DELAY", ">=": "ADD_DELAY", "<=": "ADD_DELAY"}

type SelectorBlock struct {
	BodyComponent

	Operation string
	Oi        *OperandInfo
	Inverted  bool
}

var selectorNr = 0

var selecTorOutDataWith = 1

func NewSelectorBlock(op string, oi *OperandInfo, inverted bool, parent BlockType) *SelectorBlock {
	nr := selectorNr
	selectorNr++

	name := selectorprefix + strconv.Itoa(nr)

	ret := &SelectorBlock{
		BodyComponent: BodyComponent{
			number: nr,

			archName: archPrefix + name,
			In: &HandshakeChannel{
				Out: false,
			},
			Out: &HandshakeChannel{
				Req:       name + "_o_req",
				Ack:       name + "_o_ack",
				Data:      name + "_select",
				Out:       true,
				DataWidth: selecTorOutDataWith,
			},

			parentBlock: parent,

			inputVariables:  variable.NewScopedVariables(),
			outputVariables: variable.NewScopedVariables(),

			predecessors: map[string]BodyComponentType{},
			successors:   map[string]BodyComponentType{},
		},

		Operation: op,
		Oi:        oi,
		Inverted:  inverted,
	}

	err := getInputSources(ret, parent, oi, "uint8")
	if err != nil {
		panic("getInputSource failed!")
	}

	infoPrinter.DebugPrintfln("[%s] finished getting inputs of [%s]", parent.Name(), ret.Name())

	// Add dummy successor
	ret.successors["asdf"] = nil

	return ret
}

func (sb *SelectorBlock) Name() string {
	return strings.ToLower(selectorprefix + strconv.Itoa(sb.number))
}

func (sb *SelectorBlock) ComponentStr() string { /* 	prependStr := ""
	if *sb.GetVariablesSize() > *sb.predecessor.GetVariablesSize() {
		prependStr = `(` + strconv.Itoa(*sb.GetVariablesSize()) + ` - 1 downto ` + strconv.Itoa(*sb.predecessor.GetVariablesSize()) + ` => '0') & `
	} */

	return sb.Name() + `: entity work.Selector(` + sb.ArchName() + `)
	generic map(
	  DATA_WIDTH =>  ` + strconv.Itoa(sb.inputVariables.Size) + `
	)
	port map (
	  -- Input channel
	  in_req  => ` + sb.Name() + `_in_req,
	  in_ack  => ` + sb.Name() + `_in_ack, 
	  x => ` + sb.Name() + `_x,
	  y => ` + sb.Name() + `_y,
	  -- Output channel
	  out_req =>  ` + sb.Name() + `_out_req, 
	  out_ack =>  ` + sb.Name() + `_out_ack, 
	  selector  =>  ` + sb.Name() + `_selector
	);`
	//` & (` + strconv.Itoa(*sb.GetVariablesSize()) + ` - 1 downto ` + strconv.Itoa(*sb.predecessor.GetVariablesSize()) + ` => '0'),
}

func (sb *SelectorBlock) getAliases() string {
	ret := ""
	if sb.Oi.X.IndexIdent_ == nil {
		idx := getIndex(sb.Oi.X.Index_)
		ret += "alias x      : std_logic_vector(" + strconv.Itoa(sb.Oi.X.Size_) + " - 1 downto 0)  is in_data( " + strconv.Itoa(sb.Oi.X.Position_+sb.Oi.X.Size_*(idx+1)) + " - 1 downto " + strconv.Itoa(sb.Oi.X.Position_+sb.Oi.X.Size_*idx) + ");\n"
	} else {
		ret += "signal x : std_logic_vector(" + strconv.Itoa(sb.Oi.X.Size_) + "- 1 downto 0);\n"
		ret += "constant baseX      : integer := " + strconv.Itoa(sb.Oi.X.Position_) + ";\n"
		ret += "alias offsetX      : std_logic_vector(" + strconv.Itoa(sb.Oi.X.IndexIdent_.Size_) + " - 1 downto 0)  is in_data( " + strconv.Itoa(sb.Oi.X.IndexIdent_.Position_+sb.Oi.X.IndexIdent_.Size_) + " -1 downto " + strconv.Itoa(sb.Oi.X.IndexIdent_.Position_) + ");\n"
	}

	if sb.Oi.Y != nil && sb.Oi.Y.IndexIdent_ == nil {
		idx := getIndex(sb.Oi.Y.Index_)
		ret += "alias y      : std_logic_vector(" + strconv.Itoa(sb.Oi.Y.Size_) + " - 1 downto 0)  is in_data( " + strconv.Itoa(sb.Oi.Y.Position_+sb.Oi.Y.Size_*(idx+1)) + " - 1 downto " + strconv.Itoa(sb.Oi.Y.Position_+sb.Oi.Y.Size_*idx) + ");\n"
	} else if sb.Oi.Y != nil && sb.Oi.Y.IndexIdent_ != nil {
		ret += "signal y  : std_logic_vector(" + strconv.Itoa(sb.Oi.Y.Size_) + "- 1 downto 0);\n"
		ret += "constant baseY      : integer := " + strconv.Itoa(sb.Oi.Y.Position_) + ";\n"
		ret += "alias offsetY      : std_logic_vector(" + strconv.Itoa(sb.Oi.Y.IndexIdent_.Size_) + " - 1 downto 0)  is in_data( " + strconv.Itoa(sb.Oi.Y.IndexIdent_.Position_+sb.Oi.Y.IndexIdent_.Size_) + " -1 downto " + strconv.Itoa(sb.Oi.Y.IndexIdent_.Position_) + ");\n"
	}

	return ret
}

func (sb *SelectorBlock) getCalcProcess() string {
	x := ""
	y := ""

	x = "unsigned(x)"
	if sb.Oi.X.Const_ != "" {
		x = "to_unsigned(" + sb.Oi.X.Const_ + ", " + strconv.Itoa(sb.Oi.X.Size_) + ")"
	}

	y = "unsigned(y)"
	if sb.Oi.Y.Const_ != "" {
		y = "to_unsigned(" + sb.Oi.Y.Const_ + ", " + strconv.Itoa(sb.Oi.Y.Size_) + ")"
	}

	/*
		xcalc := ""
		ycalc := ""

		 if sb.Oi.X.IndexIdent_ != nil {
			x = "unsigned(x)"
			xcalc = "x <= in_data(baseX + (to_integer(unsigned(offsetX)) + 1) * x'length - 1 downto baseX + to_integer(unsigned(offsetX)) * x'length);\n"

		}

		if sb.Oi.Y != nil && sb.Oi.Y.IndexIdent_ != nil {
			y = "unsigned(y)"
			ycalc = "y <= in_data(baseY + (to_integer(unsigned(offsetY)) + 1) * y'length - 1 downto baseY + to_integer(unsigned(offsetY)) * y'length);\n"
		}
	*/

	compute := "selector(0) <= '1' when " + x + " " + SupportedComperators[sb.Operation] + " " + y + " else '0';\n"
	if sb.Inverted {
		compute = "selector(0) <= '0' when " + x + " " + SupportedComperators[sb.Operation] + " " + y + " else '1';\n"
	}

	return /*xcalc + ycalc +*/ compute
}

func (sb *SelectorBlock) Architecture() string {

	return `architecture ` + sb.ArchName() + ` of Selector is
  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, selector: signal is "true";
	
	--attribute keep : boolean;
	--attribute keep of  x, y, selector: signal is true;
  begin
  
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ADD_DELAY  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
      );
  
    in_ack <= out_ack;
    
    ` + sb.getCalcProcess() + `

  end ` + sb.ArchName() + `;
  `
}

func (sb *SelectorBlock) EntityName() string {
	return "selector_" + sb.Name()
}

func (sb *SelectorBlock) Entity() string {
	op1Size := 0
	op2Size := 0

	if op1, err := sb.InputVariables().GetVariableInfoAt(0); err == nil {
		op1Size = op1.TotalSize()
	} else {
		panic(sb.Name() + " invalid x")
	}

	if op2, err := sb.InputVariables().GetVariableInfoAt(1); err == nil {
		op2Size = op2.TotalSize()
	} else {
		panic(sb.Name() + " invalid y")
	}

	return `LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.click_element_library_constants.ALL;

ENTITY ` + sb.EntityName() + ` IS
  GENERIC (
    DATA_WIDTH : NATURAL := 8
  );
  PORT (
    -- Data
    in_req : IN STD_LOGIC;
    in_ack : OUT STD_LOGIC;
    x : IN STD_LOGIC_VECTOR(` + strconv.Itoa(op1Size) + ` - 1 DOWNTO 0);
	y : IN STD_LOGIC_VECTOR(` + strconv.Itoa(op2Size) + ` - 1 DOWNTO 0);
    -- Selector
    out_req : OUT STD_LOGIC;
    out_ack : IN STD_LOGIC;
    selector : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
  );
END ` + sb.EntityName() + `;`
}

func (ib *SelectorBlock) GetXTotalSize() int {
	x, err := ib.InputVariables().GetVariableInfoAt(0)
	if err != nil {
		panic(ib.Name() + " invalid x")
	}

	return x.TotalSize()
}
func (ib *SelectorBlock) GetYTotalSize() int {
	y, err := ib.InputVariables().GetVariableInfoAt(1)
	if err != nil {
		panic(ib.Name() + " invalid y")
	}

	return y.TotalSize()
}

func (ib *SelectorBlock) GetSignalDefs() string {
	signalDefs := ""

	signalDefs += "signal " + ib.Name() + "_in_req : std_logic;"
	signalDefs += "signal " + ib.Name() + "_out_req : std_logic;"
	signalDefs += "signal " + ib.Name() + "_in_ack : std_logic;"
	signalDefs += "signal " + ib.Name() + "_out_ack : std_logic;"

	signalDefs += "signal " + ib.Name() + "_x : std_logic_vector(" + strconv.Itoa(ib.GetXTotalSize()) + "- 1 downto 0) := (others => '0');"
	signalDefs += "signal " + ib.Name() + "_y : std_logic_vector(" + strconv.Itoa(ib.GetYTotalSize()) + "- 1 downto 0) := (others => '0');"
	signalDefs += "signal " + ib.Name() + "_selector : std_logic_vector(0 downto 0);"

	return signalDefs
}
