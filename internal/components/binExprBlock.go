package components

import (
	"fmt"
	"go2async/internal/globalArguments"
	"go2async/pkg/variable"
	"strconv"
	"strings"
)

const binexprblockprefix = "CL_"

// TODO: check delays for more operations

var SupportedOperations map[string]string = map[string]string{"+": "+", "-": "-", "<<": "sll", ">>": "srl", "|": "or", "&": "and", "NOP": "", "=": ""}
var operationDelays map[string]string = map[string]string{"+": "ADD_DELAY", "-": "ADD_DELAY", "<<": "ADD_DELAY", ">>": "ADD_DELAY", "|": "ADD_DELAY", "&": "ADD_DELAY", "NOP": "ADD_DELAY", "=": "ADD_DELAY"}

type BinExprBlock struct {
	BodyComponent

	Nr        int
	Operation string

	Oi *OperandInfo

	opDescription string // debug
}

type OperandInfo struct {
	R, X, Y *variable.VariableInfo
}

var bebNr = 0

func NewBinExprBlock(op string, vi *OperandInfo, parent *Block) *BinExprBlock {
	nr := bebNr
	bebNr++

	name := strings.ToLower(binexprblockprefix + strconv.Itoa(nr))

	ret := &BinExprBlock{
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

		Operation: op,
		Oi:        vi,
	}

	if *globalArguments.Debug {
		opDescription := fmt.Sprintf("Creating binExprBlock %s [size %d, len %d, index %s; const %s] = %s [size %d, len %d, index %s; const %s]",
			vi.R.Name, vi.R.Size, vi.R.Len, vi.R.Index, vi.R.Const, vi.X.Name, vi.X.Size, vi.X.Len, vi.X.Index, vi.X.Const)

		if vi.Y != nil {
			opDescription += fmt.Sprintf("%s %s [size %d, len %d, index %s; const %s]", op, vi.Y.Name, vi.Y.Size, vi.Y.Len, vi.Y.Index, vi.Y.Const)
		}

		opDescription += fmt.Sprintf("\n")

		fmt.Print(opDescription)
		ret.opDescription = opDescription
	}

	return ret
}

func (bep *BinExprBlock) InChannel() *HandshakeChannel {
	return bep.In
}

func (bep *BinExprBlock) OutChannel() *HandshakeChannel {
	return bep.Out
}

func (bep *BinExprBlock) ComponentStr() string {
	name := binexprblockprefix + strconv.Itoa(bep.Nr)

	return name + `: entity work.binExprBlock(` + bep.archName + `)
	generic map(
	  DATA_WIDTH => ` + strconv.Itoa(bep.GetVariableSize()) + `
	)
	port map (
	  -- Input channel
	  in_req  => ` + bep.In.Req + `,
	  in_ack  => ` + bep.In.Ack + `, 
	  in_data => std_logic_vector(resize(unsigned(` + bep.In.Data + `), ` + strconv.Itoa(bep.GetVariableSize()) + `)),
	  -- Output channel
	  out_req => ` + bep.Out.Req + `,
	  out_ack => ` + bep.Out.Ack + `,
	  out_data  => ` + bep.Out.Data + `
	);
	`
}

func getIndex(idxStd string) int {
	idx, _ := strconv.Atoi(idxStd)
	return idx
}

func (bep *BinExprBlock) getAliases() string {
	ret := ""
	if bep.Oi.X.IndexIdent == nil {
		if bep.Oi.X.Const == "" {
			idx := getIndex(bep.Oi.X.Index)
			totalSize := bep.Oi.X.Size * bep.Oi.X.Len
			if idx > 0 {
				totalSize = bep.Oi.X.Size
			}
			ret += "alias x : std_logic_vector(" + strconv.Itoa(totalSize) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.X.Position+totalSize*(idx+1)) + " - 1 downto " + strconv.Itoa(bep.Oi.X.Position+totalSize*idx) + ");\n"
		}
	} else {
		ret += "signal x : std_logic_vector(" + strconv.Itoa(bep.Oi.X.Size) + "- 1 downto 0);\n"
		ret += "constant baseX      : integer := " + strconv.Itoa(bep.Oi.X.Position) + ";\n"
		ret += "alias offsetX      : std_logic_vector(" + strconv.Itoa(bep.Oi.X.IndexIdent.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.X.IndexIdent.Position+bep.Oi.X.IndexIdent.Size) + " -1 downto " + strconv.Itoa(bep.Oi.X.IndexIdent.Position) + ");\n"
	}

	if bep.Oi.Y != nil && bep.Oi.Y.IndexIdent == nil {
		if bep.Oi.Y.Const == "" {
			idx := getIndex(bep.Oi.Y.Index)
			totalSize := bep.Oi.Y.Size * bep.Oi.Y.Len
			if idx > 0 {
				totalSize = bep.Oi.Y.Size
			}
			ret += "alias y      : std_logic_vector(" + strconv.Itoa(totalSize) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.Y.Position+totalSize*(idx+1)) + " - 1 downto " + strconv.Itoa(bep.Oi.Y.Position+totalSize*idx) + ");\n"
		}
	} else if bep.Oi.Y != nil && bep.Oi.Y.IndexIdent != nil {
		ret += "signal y  : std_logic_vector(" + strconv.Itoa(bep.Oi.Y.Size) + "- 1 downto 0);\n"
		ret += "constant baseY      : integer := " + strconv.Itoa(bep.Oi.Y.Position) + ";\n"
		ret += "alias offsetY      : std_logic_vector(" + strconv.Itoa(bep.Oi.Y.IndexIdent.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.Y.IndexIdent.Position+bep.Oi.Y.IndexIdent.Size) + " -1 downto " + strconv.Itoa(bep.Oi.Y.IndexIdent.Position) + ");\n"
	}

	if bep.Oi.R.IndexIdent == nil {
		idx := getIndex(bep.Oi.R.Index)
		totalSize := bep.Oi.R.Size * bep.Oi.R.Len
		if idx > 0 {
			totalSize = bep.Oi.R.Size
		}
		ret += "alias result : std_logic_vector(" + strconv.Itoa(totalSize) + " - 1 downto 0)  is out_data( " + strconv.Itoa(bep.Oi.R.Position+totalSize*(idx+1)) + " - 1 downto " + strconv.Itoa(bep.Oi.R.Position+totalSize*idx) + ");\n"
	} else {
		ret += "signal result : std_logic_vector(" + strconv.Itoa(bep.Oi.R.Size) + " - 1 downto 0);\n"
		ret += "constant baseR      : integer := " + strconv.Itoa(bep.Oi.R.Position) + ";\n"
		ret += "alias offsetR      : std_logic_vector(" + strconv.Itoa(bep.Oi.R.IndexIdent.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(bep.Oi.R.IndexIdent.Position+bep.Oi.R.IndexIdent.Size) + " -1 downto " + strconv.Itoa(bep.Oi.R.IndexIdent.Position) + ");\n"
	}

	return ret
}

func (bep *BinExprBlock) getCalcProcess() string {
	x := ""
	y := ""
	delay := " after ADDER_DELAY"

	x = "unsigned(x)"
	if bep.Oi.X.Const != "" {
		x = "to_unsigned(" + bep.Oi.X.Const + ", result'length)"
	}

	if bep.Oi.Y != nil {
		y = "unsigned(y)"
		if bep.Oi.Y.Const != "" {
			y = "to_unsigned(" + bep.Oi.Y.Const + ", result'length)"
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
	variable offset: integer range 0 to out_data'length;
	begin
	out_data <= in_data; 
	`

	xcalc := ""
	ycalc := ""
	compute := ""
	resultMap := ""

	if bep.Operation != "NOP" {
		if bep.Oi.X.IndexIdent != nil {
			x = "unsigned(x)"
			xcalc = "x <= in_data(baseX + (to_integer(unsigned(offsetX)) + 1) * x'length  - 1 downto baseX + to_integer(unsigned(offsetX)) * x'length);\n"
		}

		if bep.Oi.Y != nil && bep.Oi.Y.IndexIdent != nil {
			y = "unsigned(y)"
			ycalc = "y <= in_data(baseY + (to_integer(unsigned(offsetY)) + 1) * y'length - 1 downto baseY + to_integer(unsigned(offsetY)) * y'length);\n"

		}

		compute = "result <= std_logic_vector(resize(" + x + " " + SupportedOperations[bep.Operation] + " " + y + ", result'length)) " + delay + ";\n"

		if bep.Oi.R.IndexIdent != nil {
			resultMap = "offset := baseR + to_integer(unsigned(offsetR) * result'length);\n"
			resultMap += "out_data(offset + result'length -1 downto offset) <= result;\n"
		}
	}
	return processStart + xcalc + ycalc + compute + resultMap + `
	end process;`
}

func (bep *BinExprBlock) Architecture() string {
	// TODO: analyze delays

	return `architecture ` + bep.archName + ` of binExprBlock is
	` + bep.getAliases() + `
	  
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
	return "binExprBlock"
}

func (bep *BinExprBlock) Entity() string {
	return `LIBRARY IEEE;
	USE IEEE.STD_LOGIC_1164.ALL;
	USE ieee.std_logic_unsigned.ALL;
	USE ieee.numeric_std.ALL;
	USE work.click_element_library_constants.ALL;
	
	ENTITY ` + bep.EntityName() + ` IS
	  GENERIC (
		DATA_WIDTH : NATURAL := 8
	  );
	  PORT (-- Input channel
		in_req : IN STD_LOGIC;
		in_ack : OUT STD_LOGIC;
		in_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
		-- Output channel
		out_req : OUT STD_LOGIC;
		out_ack : IN STD_LOGIC;
		out_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0));
	END ` + bep.EntityName() + `;`
}
