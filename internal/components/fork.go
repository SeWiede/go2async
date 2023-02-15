package components

import (
	"strconv"
	"strings"
)

const forkPrefix = "F_"

type Fork struct {
	BodyComponent
}

var forkNr = 0

func NewFork(parent BlockType) *Fork {
	nr := forkNr
	forkNr++

	newFork := &Fork{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch,
			name:     strings.ToLower(forkPrefix + strconv.Itoa(nr)),

			parentBlock: parent,

			inputVariables: parent.InputVariables(),
		},
	}

	newFork.In = append(newFork.In, NewInputHandshakeChannel(newFork, newFork.Name()+"_inA_req", newFork.Name()+"_inA_ack"))
	newFork.Out = append(newFork.Out, NewOutputHandshakeChannel(newFork, newFork.Name()+"_outB_req", newFork.Name()+"_outB_ack"))
	newFork.Out = append(newFork.Out, NewOutputHandshakeChannel(newFork, newFork.Name()+"_outC_req", newFork.Name()+"_outC_ack"))

	newFork.InData = append(newFork.InData, NewInDataChannel(newFork, newFork.InputVariables(), newFork.Name()+"_inA_data"))
	newFork.OutData = append(newFork.OutData, NewOutDataChannel(newFork, newFork.InputVariables(), newFork.Name()+"_outB_data"))
	newFork.OutData = append(newFork.OutData, NewOutDataChannel(newFork, newFork.InputVariables(), newFork.Name()+"_outC_data"))

	return newFork
}

func (f *Fork) Name() string {
	return f.name
}

func (f *Fork) ComponentStr() string {
	return f.Name() + `: entity work.fork
  generic map(
    DATA_WIDTH => ` + strconv.Itoa(f.InputVariables().Size) + `,
    PHASE_INIT => '0'
  )
  port map(
    -- Input Channel
    inA_req => ` + f.In[0].GetReqSignalName() + `,
    inA_ack => ` + f.In[0].GetAckSignalName() + `,
    inA_data => ` + f.InData[0].GetDataSignalName() + `,

    -- Output Channel 1
    outB_req => ` + f.Out[0].GetReqSignalName() + `,
    outB_ack => ` + f.Out[0].GetAckSignalName() + `,
    outB_data => ` + f.OutData[0].GetDataSignalName() + `,

    -- Output Channel 2
    outC_req => ` + f.Out[1].GetReqSignalName() + `,
    outC_ack => ` + f.Out[1].GetAckSignalName() + `,
    outC_data => ` + f.OutData[1].GetDataSignalName() + `,

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
	panic("fork is predefined")
}

func (f *Fork) EntityName() string {
	return "fork"
}
