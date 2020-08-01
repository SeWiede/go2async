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
	Nr       int
	archName string

	Operation string

	In  *HandshakeChannel
	Out *HandshakeChannel

	Vi       *OperandInfo
	Inverted bool
}

var selectorNr = 0

func NewSelectorBlock(op string, variableInfo *OperandInfo, inverted bool) *SelectorBlock {
	nr := selectorNr
	selectorNr++

	name := strings.ToLower(selectorprefix + strconv.Itoa(nr))
	return &SelectorBlock{
		Nr:        nr,
		archName:  archPrefix + name,
		Operation: op,
		Vi:        variableInfo,
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
	x := "unsigned(x)"
	if sb.Vi.XConstVal != "" {
		x = "to_unsigned(" + sb.Vi.XConstVal + ", " + strconv.Itoa(sb.Vi.X_SIZE) + ")"
	}

	y := "unsigned(y)"
	if sb.Vi.YConstVal != "" {
		y = "to_unsigned(" + sb.Vi.YConstVal + ", " + strconv.Itoa(sb.Vi.Y_SIZE) + ")"
	}

	compute := "'1' when " + x + " " + SupportedComperators[sb.Operation] + " " + y + " else '0';"
	if sb.Inverted {
		compute = "'0' when " + x + " " + SupportedComperators[sb.Operation] + " " + y + " else '1';"
	}

	return `architecture ` + sb.archName + ` of Selector is
    alias x      : std_logic_vector(` + strconv.Itoa(sb.Vi.X_SIZE) + ` - 1 downto 0)  is in_data( ` + strconv.Itoa(sb.Vi.X_POS+sb.Vi.X_SIZE) + ` -1 downto ` + strconv.Itoa(sb.Vi.X_POS) + `);
	alias y      : std_logic_vector(` + strconv.Itoa(sb.Vi.Y_SIZE) + ` - 1 downto 0)  is in_data( ` + strconv.Itoa(sb.Vi.Y_POS+sb.Vi.Y_SIZE) + ` -1 downto ` + strconv.Itoa(sb.Vi.Y_POS) + `);
  
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
