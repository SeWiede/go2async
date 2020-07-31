package components

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

const selectorprefix = "SE_"

// TODO: check delays for more operations

type SelectorBlock struct {
	Nr       int
	archName string

	Operation string

	In  *HandshakeChannel
	Out *HandshakeChannel

	X_POS, Y_POS         int
	XConstVal, YConstVal string
	Inverted             bool
}

var selectorNr = 0

func NewSelectorBlock(op string, X_POS, Y_POS int, XconstVal, YconstVal string, inverted bool) *SelectorBlock {
	if op == "==" {
		op = "="
	} else if op == "!=" {
		op = "/="
	} else if op == "<" || op == ">" || op == ">=" || op == "<=" {

	} else {
		fmt.Fprint(os.Stderr, "Unkown condition operator ", op, "; defaulting to '='")
		op = "="
	}

	nr := selectorNr
	selectorNr++

	name := strings.ToLower(selectorprefix + strconv.Itoa(nr))
	return &SelectorBlock{
		Nr:        nr,
		archName:  archPrefix + name,
		Operation: op,
		X_POS:     X_POS,
		Y_POS:     Y_POS,
		XConstVal: XconstVal,
		YConstVal: YconstVal,
		In: &HandshakeChannel{
			Out: false,
		},
		Out: &HandshakeChannel{
			Req:       name + "_o_req",
			Ack:       name + "_o_ack",
			Data:      name + "_select",
			DataWidth: 1,
			Out:       true,
		},
		Inverted: inverted,
	}
}

func (sb *SelectorBlock) Component() string {
	name := selectorprefix + strconv.Itoa(sb.Nr)
	return name + `: entity work.Selector(` + sb.archName + `)
	generic map(
	  VARIABLE_WIDTH => VARIABLE_WIDTH,
	  DATA_MULTIPLIER => DATA_MULTIPLIER,
	  DATA_WIDTH => DATA_WIDTH
	)
	port map (
	  -- Input channel
	  in_req  => ` + sb.In.Req + `,
	  in_ack  => ` + sb.In.Ack + `, 
	  in_data => ` + sb.In.Data + `,
	  -- Output channel
	  out_req => ` + sb.Out.Req + `,
	  out_ack => ` + sb.Out.Ack + `,
	  selector  => ` + sb.Out.Data + `
	);`
}

func (sb *SelectorBlock) Architecture() string {
	x := `unsigned(x)`
	if sb.XConstVal != "" {
		x = `to_unsigned(` + sb.XConstVal + `, VARIABLE_WIDTH)`
	}

	y := `unsigned(y)`
	if sb.YConstVal != "" {
		y = `to_unsigned(` + sb.YConstVal + `, VARIABLE_WIDTH)`
	}

	compute := `'1' when ` + x + ` ` + sb.Operation + ` ` + y + ` else '0';`
	if sb.Inverted {
		compute = `'0' when ` + x + ` ` + sb.Operation + ` ` + y + ` else '1';`
	}

	return `architecture ` + sb.archName + ` of Selector is
    alias x      : std_logic_vector(VARIABLE_WIDTH - 1 downto 0)  is in_data( ` + strconv.Itoa(sb.X_POS+1) + `*VARIABLE_WIDTH -1 downto ` + strconv.Itoa(sb.X_POS) + `*VARIABLE_WIDTH);
    alias y      : std_logic_vector(VARIABLE_WIDTH - 1 downto 0)  is in_data( ` + strconv.Itoa(sb.Y_POS+1) + `*VARIABLE_WIDTH -1 downto ` + strconv.Itoa(sb.Y_POS) + `*VARIABLE_WIDTH);
  
    attribute dont_touch : string;
    attribute dont_touch of  x,y,in_data: signal is "true";
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
    
    selector(0) <= ` + compute + `

  end ` + sb.archName + `;
  `
}

func (sb *SelectorBlock) ArchName() string {
	return sb.archName
}
