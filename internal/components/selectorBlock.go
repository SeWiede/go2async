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
	BinExprBlock

	Inverted bool

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
		BinExprBlock: BinExprBlock{
			Operation: op,
			Oi:        oi,

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
		},

		Inverted:           inverted,
		holdOffConnections: holdOffConnections,
	}

	inputChannel := NewDefaultInputHandshakeChannel(sel)
	sel.In = append(sel.In, inputChannel)

	outputChannel := NewDefaultOutputHandshakeChannel(sel)
	sel.Out = append(sel.Out, outputChannel)

	sel.InData = append(sel.InData, NewDefaultInDataChannel(sel, sel.InputVariables()))

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

	sel.Oi.R = nil

	return sel
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
	  in_data => ` + sb.InData[0].GetDataSignalName() + `,
	  -- Output channel
	  out_req => ` + sb.Out[0].GetReqSignalName() + `,
	  out_ack => ` + sb.Out[0].GetAckSignalName() + `,
	  selector  => ` + sb.OutData[0].GetDataSignalName() + `
	);`
	//` & (` + strconv.Itoa(*sb.GetVariablesSize()) + ` - 1 downto ` + strconv.Itoa(*sb.predecessor.GetVariablesSize()) + ` => '0'),
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

	compute := "selector(0) <= '1' when " + x + " " + SupportedComperators[sb.Operation] + " " + y + " else '0';\n"
	if sb.Inverted {
		compute = "selector(0) <= '0' when " + x + " " + SupportedComperators[sb.Operation] + " " + y + " else '1';\n"
	}

	return xcalc + ycalc + compute
}

func (sb *SelectorBlock) Architecture() string {

	return `architecture ` + sb.ArchName() + ` of ` + sb.EntityName() + ` is
  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, selector: signal is "true";
	
	--attribute keep : boolean;
	--attribute keep of  x, y, selector: signal is true;
	` + sb.getAliases() + `

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
    in_data : IN STD_LOGIC_VECTOR(` + strconv.Itoa(sb.InputVariables().Size) + ` - 1 DOWNTO 0);
    -- Selector
    out_req : OUT STD_LOGIC;
    out_ack : IN STD_LOGIC;
    selector : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
  );
END ` + sb.EntityName() + `;`
}
