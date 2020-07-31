package components

import (
	"fmt"
	"strconv"
	"strings"
)

const funcblockprefix = "CL_"

// TODO: check delays for more operations

var SupportedOperations map[string]string = map[string]string{"+": "+", "-": "-", "": "", "<<": "sll", ">>": "srl", "|": "or", "&": "and"}

type FuncBlock struct {
	Nr        int
	Operation string
	archName  string

	In  *HandshakeChannel
	Out *HandshakeChannel

	X_POS, Y_POS, RESULT_POS int
	XConstVal, YConstVal     string
}

var fbNr = 0

func NewFuncBlock(op string, X_POS, Y_POS, RESULT_POS int, XconstVal, YconstVal string) *FuncBlock {

	np, ok := SupportedOperations[op]
	if !ok {
		fmt.Println("Operator " + op + " not supported!")
	} else {
		op = np
	}

	nr := fbNr
	fbNr++

	name := strings.ToLower(funcblockprefix + strconv.Itoa(nr))

	return &FuncBlock{
		Nr:         nr,
		archName:   archPrefix + name,
		Operation:  op,
		X_POS:      X_POS,
		Y_POS:      Y_POS,
		RESULT_POS: RESULT_POS,
		XConstVal:  XconstVal,
		YConstVal:  YconstVal,
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
      VARIABLE_WIDTH => VARIABLE_WIDTH,
	  DATA_MULTIPLIER => DATA_MULTIPLIER,
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
	x := `unsigned(x)`
	if fb.XConstVal != "" {
		x = `to_unsigned(` + fb.XConstVal + `, VARIABLE_WIDTH)`
	}

	y := `unsigned(y)`
	if fb.YConstVal != "" {
		y = `to_unsigned(` + fb.YConstVal + `, VARIABLE_WIDTH)`
	}

	if fb.Operation == "srl" || fb.Operation == "sll" {
		y = "to_integer(" + y + ")"
	}

	delay := " after ADDER_DELAY"
	if fb.Y_POS < 0 || fb.Operation == "" {
		y = ``
		delay = ``
		fb.Operation = ""
	}

	compute := "std_logic_vector(" + x + " " + fb.Operation + " " + y + ") " + delay

	return `architecture ` + fb.archName + ` of funcBlock is

    alias result : std_logic_vector(VARIABLE_WIDTH - 1 downto 0)  is out_data( ` + strconv.Itoa(fb.RESULT_POS+1) + `*VARIABLE_WIDTH -1 downto ` + strconv.Itoa(fb.RESULT_POS) + `*VARIABLE_WIDTH);
    alias x      : std_logic_vector(VARIABLE_WIDTH - 1 downto 0)  is in_data( ` + strconv.Itoa(fb.X_POS+1) + `*VARIABLE_WIDTH -1 downto ` + strconv.Itoa(fb.X_POS) + `*VARIABLE_WIDTH);
    alias y      : std_logic_vector(VARIABLE_WIDTH - 1 downto 0)  is in_data( ` + strconv.Itoa(fb.Y_POS+1) + `*VARIABLE_WIDTH -1 downto ` + strconv.Itoa(fb.Y_POS) + `*VARIABLE_WIDTH);
  begin
    in_ack <= out_ack;
    
    delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => ADD_DELAY  -- Delay  size
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
