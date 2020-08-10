package components

import (
	"go2async/pkg/variable"
	"strconv"
	"strings"
)

const funcblockprefix = "CL_"

// TODO: check delays for more operations

var SupportedOperations map[string]string = map[string]string{"+": "+", "-": "-", "<<": "sll", ">>": "srl", "|": "or", "&": "and", "NOP": "", "=": ""}
var operationDelays map[string]string = map[string]string{"+": "ADD_DELAY", "-": "ADD_DELAY", "<<": "ADD_DELAY", ">>": "ADD_DELAY", "|": "ADD_DELAY", "&": "ADD_DELAY", "NOP": "ADD_DELAY", "=": "ADD_DELAY"}

type FuncBlock struct {
	Nr        int
	Operation string
	archName  string

	In  *HandshakeChannel
	Out *HandshakeChannel

	Oi *OperandInfo
}

type OperandInfo struct {
	R, X, Y *variable.VariableInfo
}

var fbNr = 0

func NewFuncBlock(op string, vi *OperandInfo) *FuncBlock {
	nr := fbNr
	fbNr++

	name := strings.ToLower(funcblockprefix + strconv.Itoa(nr))

	return &FuncBlock{
		Nr:        nr,
		archName:  archPrefix + name,
		Operation: op,
		Oi:        vi,
		In: &HandshakeChannel{
			Out: false,
		},
		Out: &HandshakeChannel{
			Req:  name + "_o_req",
			Ack:  name + "_o_ack",
			Data: name + "_data",
			Out:  true,
		},
	}
}

func (fb *FuncBlock) InChannel() *HandshakeChannel {
	return fb.In
}

func (fb *FuncBlock) OutChannel() *HandshakeChannel {
	return fb.Out
}

func (fb *FuncBlock) Component() string {
	name := funcblockprefix + strconv.Itoa(fb.Nr)
	return name + `: entity work.funcBlock(` + fb.archName + `)
	generic map(
	  DATA_WIDTH => DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => ` + fb.In.Req + `,
	  in_ack  => ` + fb.In.Ack + `, 
	  in_data => ` + fb.In.Data + `,
	  -- Output channel
	  out_req => ` + fb.Out.Req + `,
	  out_ack => ` + fb.Out.Ack + `,
	  out_data  => ` + fb.Out.Data + `
	);
	`
}

func getIndex(idxStd string) int {
	idx, _ := strconv.Atoi(idxStd)
	return idx
}

func (fb *FuncBlock) getAliases() string {
	ret := ""
	if fb.Oi.X.IndexIdent == nil {
		idx := getIndex(fb.Oi.X.Index)
		ret += "alias x      : std_logic_vector(" + strconv.Itoa(fb.Oi.X.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(fb.Oi.X.Position+fb.Oi.X.Size*(idx+1)) + " - 1 downto " + strconv.Itoa(fb.Oi.X.Position+fb.Oi.X.Size*idx) + ");\n"
	} else {
		ret += "signal x : std_logic_vector(" + strconv.Itoa(fb.Oi.X.Size) + "- 1 downto 0);\n"
		ret += "constant baseX      : integer := " + strconv.Itoa(fb.Oi.X.Position) + ";\n"
		ret += "alias offsetX      : std_logic_vector(" + strconv.Itoa(fb.Oi.X.IndexIdent.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(fb.Oi.X.IndexIdent.Position+fb.Oi.X.IndexIdent.Size) + " -1 downto " + strconv.Itoa(fb.Oi.X.IndexIdent.Position) + ");\n"
	}

	if fb.Oi.Y != nil && fb.Oi.Y.IndexIdent == nil {
		idx := getIndex(fb.Oi.Y.Index)
		ret += "alias y      : std_logic_vector(" + strconv.Itoa(fb.Oi.Y.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(fb.Oi.Y.Position+fb.Oi.Y.Size*(idx+1)) + " - 1 downto " + strconv.Itoa(fb.Oi.Y.Position+fb.Oi.Y.Size*idx) + ");\n"
	} else if fb.Oi.Y != nil && fb.Oi.Y.IndexIdent != nil {
		ret += "signal y  : std_logic_vector(" + strconv.Itoa(fb.Oi.Y.Size) + "- 1 downto 0);\n"
		ret += "constant baseY      : integer := " + strconv.Itoa(fb.Oi.Y.Position) + ";\n"
		ret += "alias offsetY      : std_logic_vector(" + strconv.Itoa(fb.Oi.Y.IndexIdent.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(fb.Oi.Y.IndexIdent.Position+fb.Oi.Y.IndexIdent.Size) + " -1 downto " + strconv.Itoa(fb.Oi.Y.IndexIdent.Position) + ");\n"
	}

	if fb.Oi.R.IndexIdent == nil {
		idx := getIndex(fb.Oi.R.Index)
		ret += "alias result : std_logic_vector(" + strconv.Itoa(fb.Oi.R.Size) + " - 1 downto 0)  is out_data( " + strconv.Itoa(fb.Oi.R.Position+fb.Oi.R.Size*(idx+1)) + " - 1 downto " + strconv.Itoa(fb.Oi.R.Position+fb.Oi.R.Size*idx) + ");\n"
	} else {
		ret += "signal result : std_logic_vector(" + strconv.Itoa(fb.Oi.R.Size) + " - 1 downto 0);\n"
		ret += "constant baseR      : integer := " + strconv.Itoa(fb.Oi.R.Position) + ";\n"
		ret += "alias offsetR      : std_logic_vector(" + strconv.Itoa(fb.Oi.R.IndexIdent.Size) + " - 1 downto 0)  is in_data( " + strconv.Itoa(fb.Oi.R.IndexIdent.Position+fb.Oi.R.IndexIdent.Size) + " -1 downto " + strconv.Itoa(fb.Oi.R.IndexIdent.Position) + ");\n"
	}

	return ret
}

func (fb *FuncBlock) getCalcProcess() string {
	x := ""
	y := ""
	delay := " after ADDER_DELAY"

	x = "unsigned(x)"
	if fb.Oi.X.Const != "" {
		x = "to_unsigned(" + fb.Oi.X.Const + ", x'length)"
	}

	if fb.Oi.Y != nil {
		y = "unsigned(y)"
		if fb.Oi.Y.Const != "" {
			y = "to_unsigned(" + fb.Oi.Y.Const + ", y'length)"
		}

		if fb.Operation == "<<" || fb.Operation == ">>" {
			y = "to_integer(" + y + ")"
		}
	}

	if fb.Operation == "NOP" || fb.Operation == "=" || fb.Oi.Y == nil {
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

	if fb.Operation != "NOP" {
		if fb.Oi.X.IndexIdent != nil {
			x = "unsigned(x)"
			xcalc = "x <= in_data(baseX + (to_integer(unsigned(offsetX)) + 1) * x'length  - 1 downto baseX + to_integer(unsigned(offsetX)) * x'length);\n"
		}

		if fb.Oi.Y != nil && fb.Oi.Y.IndexIdent != nil {
			y = "unsigned(y)"
			ycalc = "y <= in_data(baseY + (to_integer(unsigned(offsetY)) + 1) * y'length - 1 downto baseY + to_integer(unsigned(offsetY)) * y'length);\n"

		}

		compute = "result <= std_logic_vector(resize(" + x + " " + SupportedOperations[fb.Operation] + " " + y + ", result'length)) " + delay + ";\n"

		if fb.Oi.R.IndexIdent != nil {
			resultMap = "offset := baseR + to_integer(unsigned(offsetR) * result'length);\n"
			resultMap += "out_data(offset + result'length -1 downto offset) <= result;\n"
		}
	}
	return processStart + xcalc + ycalc + compute + resultMap + `
	end process;`
}

func (fb *FuncBlock) Architecture() string {
	// TODO: analyze delays

	return `architecture ` + fb.archName + ` of funcBlock is
	` + fb.getAliases() + `
	  
    --attribute dont_touch : string;
	--attribute dont_touch of  x, y, result: signal is "true";
	  
    --attribute keep : boolean;
	--attribute keep of  x, y, result: signal is true;
  begin
    in_ack <= out_ack;
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ` + operationDelays[fb.Operation] + `  -- Delay  size
      )
      port map (
        i => in_req,
        o => out_req
	  );
	  

	  ` + fb.getCalcProcess() + `
  end ` + fb.archName + `;
  `
}

func (fb *FuncBlock) ArchName() string {
	return fb.archName
}
