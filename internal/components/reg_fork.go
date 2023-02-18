package components

import (
	"strconv"
	"strings"
)

const regForkPrefix = "RF_"

type RegFork struct {
	BodyComponent
}

var regForkNr = 0

func NewRegFork(parent BlockType) *RegFork {
	nr := regForkNr
	regForkNr++

	newReg := &RegFork{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch,
			name:     strings.ToLower(regForkPrefix + strconv.Itoa(nr)),

			parentBlock: parent,

			inputVariables:  parent.InputVariables(),
			outputVariables: parent.InputVariables(),
		},
	}

	newReg.In = append(newReg.In, NewInputHandshakeChannel(newReg, newReg.Name()+"_inA_req", newReg.Name()+"_inA_ack"))
	newReg.Out = append(newReg.Out, NewOutputHandshakeChannel(newReg, newReg.Name()+"_outB_req", newReg.Name()+"_outB_ack"))
	newReg.Out = append(newReg.Out, NewOutputHandshakeChannel(newReg, newReg.Name()+"_outC_req", newReg.Name()+"_outC_ack"))

	newReg.InData = append(newReg.InData, NewInDataChannel(newReg, newReg.InputVariables(), newReg.Name()+"_inA_data"))
	newReg.OutData = append(newReg.OutData, NewOutDataChannel(newReg, newReg.InputVariables(), newReg.Name()+"_outB_data"))
	newReg.OutData = append(newReg.OutData, NewOutDataChannel(newReg, newReg.InputVariables(), newReg.Name()+"_outC_data"))

	//name := strings.ToLower(regForkPrefix + strconv.Itoa(nr))
	return newReg
}

func (rf *RegFork) Name() string {
	return rf.name
}

func (rf *RegFork) ComponentStr() string {
	return rf.Name() + `: entity work.reg_fork
  generic map(
    DATA_WIDTH => ` + strconv.Itoa(rf.InputVariables().Size) + `,
    PHASE_INIT_A => '0',
    PHASE_INIT_B =>'0',
    PHASE_INIT_C => '0')
  port map (
    -- Input Channel
    inA_req => ` + rf.In[0].GetReqSignalName() + `,
    inA_ack => ` + rf.In[0].GetAckSignalName() + `,
    inA_data => ` + rf.InData[0].GetDataSignalName() + `,

    -- Output Channel 1
    outB_req => ` + rf.Out[0].GetReqSignalName() + `,
    outB_ack => ` + rf.Out[0].GetAckSignalName() + `,
    outB_data => ` + rf.OutData[0].GetDataSignalName() + `,

    -- Output Channel 2
    outC_req => ` + rf.Out[1].GetReqSignalName() + `,
    outC_ack => ` + rf.Out[1].GetAckSignalName() + `,
    outC_data => ` + rf.OutData[1].GetDataSignalName() + `,
    
    rst => rst
  );`
}

func (rf *RegFork) Architecture() string {
	return `architecture ` + rf.archName + ` of reg_fork is

  signal click: std_logic;
  signal phase_a: std_logic;
  signal phase_b, phase_c, outB_bubble, outC_bubble, inA_token: std_logic;
  signal data_reg: std_logic_vector(DATA_WIDTH-1 downto 0);
  
  attribute dont_touch : string;
  attribute dont_touch of  phase_b, phase_a, phase_c : signal is "true";   
  attribute dont_touch of  data_reg : signal is "true";  
  attribute dont_touch of  click : signal is "true";
  
  
  begin
    inA_token <= inA_req xor phase_a after XOR_DELAY;
    outB_bubble <= phase_b xnor outB_ack after XOR_DELAY + NOT1_DELAY;
    outC_bubble <= phase_c xnor outC_ack after XOR_DELAY + NOT1_DELAY;
    -------------------------------------------------------
  
    click <= inA_token and outB_bubble and outC_bubble after AND3_DELAY;
  
    clock_regs: process(click, rst)
    begin
      if rst = '1' then
        phase_a <= PHASE_INIT_A;
        phase_b <= PHASE_INIT_B;
        phase_c <= PHASE_INIT_C;
        data_reg <= std_logic_vector(to_unsigned(VALUE, DATA_WIDTH));
      elsif rising_edge(click) then
        phase_a <= not phase_a after REG_CQ_DELAY;
        phase_b <= not phase_b after REG_CQ_DELAY;
        phase_c <= not phase_c after REG_CQ_DELAY;
        data_reg <= inA_data after REG_CQ_DELAY;
      end if;
    end process clock_regs;
  
    inA_ack <= phase_a;
    outB_req <= phase_b;
    outC_req <= phase_c;
    outB_data <= data_reg;
    outC_data <= data_reg;
  
  end ` + rf.archName + `;
  `
}

func (rf *RegFork) ArchName() string {
	return rf.archName
}

func (rf *RegFork) Entity() string {
	panic("regfork is predefined")
}

func (rf *RegFork) EntityName() string {
	return "regfork"
}
