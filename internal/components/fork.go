package components

import (
	"go2async/internal/variable"
	"strconv"
	"strings"
)

const forkPrefix = "F_"

type Fork struct {
	BodyComponent

	In   *HandshakeChannel
	Out1 *HandshakeChannel
	Out2 *HandshakeChannel
}

var forkNr = 0

func NewFork(inputVariables *variable.ScopedVariables) *Fork {
	if inputVariables == nil {
		panic("nil inputvariables")
	}

	nr := forkNr
	forkNr++

	return &Fork{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch,

			inputVariables: inputVariables,
		},

		/* In: &HandshakeChannel{
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
		}, */
	}
}

func (f *Fork) Name() string {
	return strings.ToLower(forkPrefix + strconv.Itoa(f.number))
}

func (f *Fork) ComponentStr() string {
	return f.Name() + `: entity work.fork
  generic map(
    DATA_WIDTH => ` + strconv.Itoa(f.InputVariables().Size) + `,
    PHASE_INIT => '0'
  )
  port map(
    -- Input Channel
    inA_req => ` + f.Name() + `_inA_req,
    inA_ack => ` + f.Name() + `_inA_ack,
    inA_data => ` + f.Name() + `_inA_data,

    -- Output Channel 1
    outB_req => ` + f.Name() + `_outB_req,
    outB_ack => ` + f.Name() + `_outB_ack,
    outB_data => ` + f.Name() + `_out_B_data,

    -- Output Channel 2
    outC_req => ` + f.Name() + `_outC_req,
    outC_ack => ` + f.Name() + `_outC_ack,
    outC_data => ` + f.Name() + `_outC_data,

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

func (f *Fork) Entity() string {
	panic("demux is predefined")
}

func (f *Fork) EntityName() string {
	return "demux"
}

func (f *Fork) GetSignalDefs() string {
	panic("signaldefs not implemented")
	return ""
}
