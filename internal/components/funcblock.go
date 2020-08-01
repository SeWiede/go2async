package components

import (
	"strconv"
	"strings"
)

const funcblockprefix = "CL_"

// TODO: check delays for more operations

var SupportedOperations map[string]string = map[string]string{"+": "+", "-": "-", "": "", "<<": "sll", ">>": "srl", "|": "or", "&": "and", "NOP": "", "=": ""}
var operationDelays map[string]string = map[string]string{"+": "ADD_DELAY", "-": "ADD_DELAY", "": "", "<<": "ADD_DELAY", ">>": "ADD_DELAY", "|": "ADD_DELAY", "&": "ADD_DELAY", "NOP": "0", "=": "0"}

/*
 --Delay size
  CONSTANT ADD_DELAY : INTEGER := 15;
  CONSTANT LUT_CHAIN_SIZE : INTEGER := 10;
  CONSTANT AND2_DELAY : TIME := 2 ns; -- 2 input AND gate
  CONSTANT AND3_DELAY : TIME := 3 ns; -- 3 input AND gate
  CONSTANT NOT1_DELAY : TIME := 1 ns; -- 1 input NOT gate
  CONSTANT ANDOR3_DELAY : TIME := 4 ns; -- Complex AND_OR gate
  CONSTANT REG_CQ_DELAY : TIME := 1 ns; -- Clk to Q delay
  CONSTANT ADDER_DELAY : TIME := 15 ns; -- Adder delay

  CONSTANT OR2_DELAY : TIME := 2 ns; -- 2 input OR gate
  CONSTANT XOR_DELAY : TIME := 3 ns; --2 input XOR gate*/

type FuncBlock struct {
	Nr        int
	Operation string
	archName  string

	In  *HandshakeChannel
	Out *HandshakeChannel

	Vi *OperandInfo
}

type OperandInfo struct {
	X_POS, Y_POS, RESULT_POS    int
	X_SIZE, Y_SIZE, RESULT_SIZE int
	XConstVal, YConstVal        string
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
		Vi:        vi,
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

func (fb *FuncBlock) Architecture() string {
	x := "unsigned(x)"
	if fb.Vi.XConstVal != "" {
		x = "to_unsigned(" + fb.Vi.XConstVal + ", " + strconv.Itoa(fb.Vi.X_SIZE) + ")"
	}

	y := "unsigned(y)"
	if fb.Vi.YConstVal != "" {
		y = "to_unsigned(" + fb.Vi.YConstVal + ", " + strconv.Itoa(fb.Vi.Y_SIZE) + ")"
	}

	delay := " after ADDER_DELAY"
	if fb.Operation == "<<" || fb.Operation == ">>" {
		y = "to_integer(" + y + ")"
	} else if fb.Operation == "NOP" || fb.Operation == "=" {
		y = ""
		delay = ""
	}

	compute := "std_logic_vector(resize(" + x + " " + SupportedOperations[fb.Operation] + " " + y + ", " + strconv.Itoa(fb.Vi.RESULT_SIZE) + ")) " + delay

	return `architecture ` + fb.archName + ` of funcBlock is
    alias x      : std_logic_vector(` + strconv.Itoa(fb.Vi.X_SIZE) + ` - 1 downto 0)  is in_data( ` + strconv.Itoa(fb.Vi.X_POS+fb.Vi.X_SIZE) + ` -1 downto ` + strconv.Itoa(fb.Vi.X_POS) + `);
	alias y      : std_logic_vector(` + strconv.Itoa(fb.Vi.Y_SIZE) + ` - 1 downto 0)  is in_data( ` + strconv.Itoa(fb.Vi.Y_POS+fb.Vi.Y_SIZE) + ` -1 downto ` + strconv.Itoa(fb.Vi.Y_POS) + `);
	alias result : std_logic_vector(` + strconv.Itoa(fb.Vi.RESULT_SIZE) + ` - 1 downto 0)  is out_data( ` + strconv.Itoa(fb.Vi.RESULT_POS+fb.Vi.RESULT_SIZE) + ` -1 downto ` + strconv.Itoa(fb.Vi.RESULT_POS) + `);
	  
    attribute dont_touch : string;
	attribute dont_touch of  x, y, result: signal is "true";
	attribute preserve : BOOLEAN;
	attribute preserve of x, y, result : signal is true;
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
	  process(all)
	  begin
		out_data <= in_data;  
		result <= ` + compute + `; 
	  end process;
  end ` + fb.archName + `;
  `
}

func (fb *FuncBlock) ArchName() string {
	return fb.archName
}
