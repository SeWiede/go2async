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

	addedInput int

	holdOffConnections bool
}

var selectorNr = 0

var selecTorOutDataWith = 1

func NewSelectorBlock(op string, oi *OperandInfo, inverted bool, holdOffConnections bool, parent BlockType) *SelectorBlock {
	nr := selectorNr
	selectorNr++

	name := selectorprefix + strconv.Itoa(nr)

	sel := &SelectorBlock{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: archPrefix + name,
			name:     strings.ToLower(selectorprefix + strconv.Itoa(nr)),

			parentBlock: parent,

			inputVariables:  variable.NewScopedVariables(),
			outputVariables: variable.NewScopedVariables(),

			predecessors: map[string]BodyComponentType{},
			successors:   map[string]BodyComponentType{},
		},

		Operation: op,
		Oi:        oi,
		Inverted:  inverted,

		holdOffConnections: holdOffConnections,
	}

	inputChannel := NewDefaultInputHandshakeChannel(sel)
	sel.In = append(sel.In, inputChannel)

	outputChannel := NewDefaultOutputHandshakeChannel(sel)
	sel.Out = append(sel.Out, outputChannel)

	sel.InData = append(sel.InData, NewDataChannel(sel, variable.NewScopedVariables(), sel.Name()+"_x", false))
	sel.InData = append(sel.InData, NewDataChannel(sel, variable.NewScopedVariables(), sel.Name()+"_y", false))

	sel.OutData = append(sel.OutData, NewDataChannel(sel, sel.OutputVariables(), sel.Name()+"_selector", true))

	err := getInputSources(sel, parent, oi, "uint8", holdOffConnections)
	if err != nil {
		panic("getInputSource failed!")
	}

	_, err = sel.outputVariables.AddVariable(&variable.VariableTypeDecl{
		Name_: "__go2async_selector__var",
		Typ_:  "__go2async_selector",
		Len_:  1,
	})
	if err != nil {
		panic(err.Error())
	}

	infoPrinter.DebugPrintfln("[%s] finished getting inputs of [%s]", parent.Name(), sel.Name())

	return sel
}

func (sb *SelectorBlock) AddInputVariable(vtd *variable.VariableInfo) (*variable.VariableInfo, error) {
	// Allow both inputs to be the same
	vi, err := sb.InputVariables().AddVariable(vtd)
	if err != nil {
		vi, err = sb.InputVariables().GetVariableInfo(vtd.Name())
		if err != nil {
			return nil, err
		}
	}

	// Add inputs to their respecting inputChannels
	if sb.addedInput >= 2 {
		panic("cannot add more than 2 inputs to binExpr")
	} else if sb.addedInput == 1 {
		sb.InDataChannels()[1].variables.AddVariable(vtd)
	} else {
		sb.InDataChannels()[0].variables.AddVariable(vtd)

	}
	sb.addedInput++

	if !sb.isBlock {
		// Check owner map
		parent := sb.parentBlock
		if _, ok := parent.GetVariableOwnerMap()[vi.Name()]; !ok {

			parent.GetVariableOwnerMap()[vi.Name()] = &variableOwner{
				ownerList: NewOwnerList(parent),
				vi:        vtd,
			}

			infoPrinter.DebugPrintfln("[%s]: No owner for variable '%s' found. Making parent %s owner", sb.archName, vi.Name(), parent.Name())
		}
	}

	return vi, err
}

func (sb *SelectorBlock) Name() string {
	return sb.name
}

func (sb *SelectorBlock) ComponentStr() string { /* 	prependStr := ""
	if *sb.GetVariablesSize() > *sb.predecessor.GetVariablesSize() {
		prependStr = `(` + strconv.Itoa(*sb.GetVariablesSize()) + ` - 1 downto ` + strconv.Itoa(*sb.predecessor.GetVariablesSize()) + ` => '0') & `
	} */

	return sb.Name() + `: entity work.` + sb.EntityName() + `(` + sb.ArchName() + `)
	generic map(
	  DATA_WIDTH =>  ` + strconv.Itoa(sb.inputVariables.Size) + `
	)
	port map (
	  -- Input channel
	  in_req  => ` + sb.In[0].GetReqSignalName() + `,
	  in_ack  => ` + sb.In[0].GetAckSignalName() + `, 
	  x => ` + sb.InData[0].GetDataSignalName() + `,
	  y => ` + sb.InData[1].GetDataSignalName() + `,
	  -- Output channel
	  out_req => ` + sb.Out[0].GetReqSignalName() + `,
	  out_ack => ` + sb.Out[0].GetAckSignalName() + `,
	  selector  => ` + sb.OutData[0].GetDataSignalName() + `
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

	return `architecture ` + sb.ArchName() + ` of ` + sb.EntityName() + ` is
  
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
