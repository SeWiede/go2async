package components

import (
	"strconv"
	"strings"
)

const forkPrefix = "F_"

type Fork struct {
	Nr       int
	archName string

	DataWidth string

	In   *HandshakeChannel
	Out1 *HandshakeChannel
	Out2 *HandshakeChannel
}

var forkNr = 0

func NewFork(datawdith string) *Fork {
	nr := forkNr
	forkNr++

	if datawdith == "" {
		datawdith = "DATA_WIDTH"
	}

	name := strings.ToLower(forkPrefix + strconv.Itoa(nr))
	return &Fork{
		Nr:        nr,
		archName:  defaultArch,
		DataWidth: datawdith,
		In: &HandshakeChannel{
			Out: false,
		},
		Out1: &HandshakeChannel{
			Req:  name + "_b_o_req",
			Ack:  name + "_b_o_ack",
			Data: name + "_b_data",
			Out:  true,
		},
		Out2: &HandshakeChannel{
			Req:  name + "_c_o_req",
			Ack:  name + "_c_o_ack",
			Data: name + "_c_data",
			Out:  true,
		},
	}
}

func (f *Fork) Component() string {
	name := forkPrefix + strconv.Itoa(f.Nr)
	return name + `: entity work.fork
  generic map(
    DATA_WIDTH => ` + f.DataWidth + `,
    PHASE_INIT => '0'
  )
  port map(
    inA_ack => ` + f.In.Ack + `,
    inA_req => ` + f.In.Req + `,
    inA_data => ` + f.In.Data + `,
    -- Output Channel
    outB_ack => ` + f.Out1.Ack + `,
    outB_req => ` + f.Out1.Req + `,
    outB_data => ` + f.Out1.Data + `,
    outC_ack => ` + f.Out2.Ack + `,
    outC_req => ` + f.Out2.Req + `,
    outC_data => ` + f.Out2.Data + `,
    rst => rst
  );
    `
}

func (f *Fork) Architecture() string {
	return `architecture ` + f.archName + ` of fork is

  signal click: std_logic;
  signal phase: std_logic := PHASE_INIT;
  
  attribute dont_touch : string;
  attribute dont_touch of  phase : signal is "true";   
  attribute dont_touch of  click : signal is "true"; 

begin
  -- Control Path
  outB_req <= inA_req;
  outC_req <= inA_req;
  outB_data <= inA_data;
  outC_data <= inA_data
  inA_ack <= phase;

  click <= (outC_ack and outB_ack and not(phase)) or (not(outC_ack) and not(outB_ack) and phase) after AND3_DELAY + OR2_DELAY; 

  clock_regs: process(click, rst)
  begin
    if rst = '1' then
      phase <= PHASE_INIT;
    else
      if rising_edge(click) then
        phase <= not phase after REG_CQ_DELAY;
      end if;
    end if;
  end process clock_regs;
    
end ` + f.archName + `;
`
}

func (f *Fork) ArchName() string {
	return f.archName
}
