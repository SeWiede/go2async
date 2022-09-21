package components

import (
	"strconv"
	"strings"
)

const selectorprefix = "SE_"

// TODO: check delays
var SupportedComperators map[string]string = map[string]string{"==": "=", "!=": "/=", "<": "<", ">": ">", ">=": ">=", "<=": "<="}
var comperatorDelays map[string]string = map[string]string{"==": "ADD_DELAY", "!=": "ADD_DELAY", "<": "ADD_DELAY", ">": "ADD_DELAY", ">=": "ADD_DELAY", "<=": "ADD_DELAY"}

type SelectorBlock struct {
	BodyComponent

	Nr int

	Operation string
	Oi        *OperandInfo
	Inverted  bool
}

var selectorNr = 0

var selecTorOutDataWith = 1

func NewSelectorBlock(op string, variableInfo *OperandInfo, inverted bool, parent *Block) *SelectorBlock {
	nr := selectorNr
	selectorNr++

	name := strings.ToLower(selectorprefix + strconv.Itoa(nr))

	ret := &SelectorBlock{
		BodyComponent: BodyComponent{
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

			parent:       parent,
			variableSize: parent.GetCurrentVariableSize(),
		},

		Nr: nr,

		Operation: op,
		Oi:        variableInfo,
		Inverted:  inverted,
	}

	return ret
}

func (sb *SelectorBlock) ComponentStr() string {
	name := selectorprefix + strconv.Itoa(sb.Nr)

	/* 	prependStr := ""
	if *sb.GetVariablesSize() > *sb.predecessor.GetVariablesSize() {
		prependStr = `(` + strconv.Itoa(*sb.GetVariablesSize()) + ` - 1 downto ` + strconv.Itoa(*sb.predecessor.GetVariablesSize()) + ` => '0') & `
	} */

	return name + `: entity work.Selector(` + sb.archName + `)
	generic map(
	  DATA_WIDTH =>  DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => ` + sb.In.Req + `,
	  in_ack  => ` + sb.In.Ack + `, 
	  in_data =>  std_logic_vector(resize(unsigned(` + sb.In.Data + `), ` + strconv.Itoa(sb.GetVariableSize()) + `)),
	  -- Output channel
	  out_req => ` + sb.Out.Req + `,
	  out_ack => ` + sb.Out.Ack + `,
	  selector  => ` + sb.Out.Data + `
	  );`
	//` & (` + strconv.Itoa(*sb.GetVariablesSize()) + ` - 1 downto ` + strconv.Itoa(*sb.predecessor.GetVariablesSize()) + ` => '0'),
}

func (sb *SelectorBlock) getAliases() string {
	ret := ""
	if sb.Oi.X.IndexIdent == nil {
		idx := getIndex(sb.Oi.X.Index)
		ret += "alias x      : std_logic_vector(" + strconv.Itoa(sb.Oi.X.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(sb.Oi.X.Position+sb.Oi.X.Size*(idx+1)) + " - 1 downto " + strconv.Itoa(sb.Oi.X.Position+sb.Oi.X.Size*idx) + ");\n"
	} else {
		ret += "signal x : std_logic_vector(" + strconv.Itoa(sb.Oi.X.Size) + "- 1 downto 0);\n"
		ret += "constant baseX      : integer := " + strconv.Itoa(sb.Oi.X.Position) + ";\n"
		ret += "alias offsetX      : std_logic_vector(" + strconv.Itoa(sb.Oi.X.IndexIdent.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(sb.Oi.X.IndexIdent.Position+sb.Oi.X.IndexIdent.Size) + " -1 downto " + strconv.Itoa(sb.Oi.X.IndexIdent.Position) + ");\n"
	}

	if sb.Oi.Y != nil && sb.Oi.Y.IndexIdent == nil {
		idx := getIndex(sb.Oi.Y.Index)
		ret += "alias y      : std_logic_vector(" + strconv.Itoa(sb.Oi.Y.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(sb.Oi.Y.Position+sb.Oi.Y.Size*(idx+1)) + " - 1 downto " + strconv.Itoa(sb.Oi.Y.Position+sb.Oi.Y.Size*idx) + ");\n"
	} else if sb.Oi.Y != nil && sb.Oi.Y.IndexIdent != nil {
		ret += "signal y  : std_logic_vector(" + strconv.Itoa(sb.Oi.Y.Size) + "- 1 downto 0);\n"
		ret += "constant baseY      : integer := " + strconv.Itoa(sb.Oi.Y.Position) + ";\n"
		ret += "alias offsetY      : std_logic_vector(" + strconv.Itoa(sb.Oi.Y.IndexIdent.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(sb.Oi.Y.IndexIdent.Position+sb.Oi.Y.IndexIdent.Size) + " -1 downto " + strconv.Itoa(sb.Oi.Y.IndexIdent.Position) + ");\n"
	}

	return ret
}

func (sb *SelectorBlock) getCalcProcess() string {
	x := ""
	y := ""

	x = "unsigned(x)"
	if sb.Oi.X.Const != "" {
		x = "to_unsigned(" + sb.Oi.X.Const + ", " + strconv.Itoa(sb.Oi.X.Size) + ")"
	}

	y = "unsigned(y)"
	if sb.Oi.Y.Const != "" {
		y = "to_unsigned(" + sb.Oi.Y.Const + ", " + strconv.Itoa(sb.Oi.Y.Size) + ")"
	}

	xcalc := ""
	ycalc := ""

	if sb.Oi.X.IndexIdent != nil {
		x = "unsigned(x)"
		xcalc = "x <= in_data(baseX + (to_integer(unsigned(offsetX)) + 1) * x'length - 1 downto baseX + to_integer(unsigned(offsetX)) * x'length);\n"

	}

	if sb.Oi.Y != nil && sb.Oi.Y.IndexIdent != nil {
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

	return `architecture ` + sb.archName + ` of Selector is
	` + sb.getAliases() + `
  
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

  end ` + sb.archName + `;
  `
}

func (sb *SelectorBlock) EntityName() string {
	return "Selector"
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
    in_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    in_req : IN STD_LOGIC;
    in_ack : OUT STD_LOGIC;
    -- Selector
    selector : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    out_req : OUT STD_LOGIC;
    out_ack : IN STD_LOGIC
  );
END ` + sb.EntityName() + `;`
}
