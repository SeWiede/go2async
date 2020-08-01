package components

import (
	"strconv"
	"strings"
)

const regForkPrefix = "RF_"

type RegFork struct {
	Nr       int
	archName string

	In   *HandshakeChannel
	Out1 *HandshakeChannel
	Out2 *HandshakeChannel
}

var regForkNr = 0

func NewRegFork() *RegFork {
	nr := regForkNr
	regForkNr++

	name := strings.ToLower(regForkPrefix + strconv.Itoa(nr))
	return &RegFork{
		Nr:       nr,
		archName: defaultArch,
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

func (rf *RegFork) Component() string {
	name := regForkPrefix + strconv.Itoa(rf.Nr)
	return name + `: entity work.reg_fork
  generic map(
    DATA_WIDTH => DATA_WIDTH,
    PHASE_INIT_A => '0',
    PHASE_INIT_B =>'0',
    PHASE_INIT_C => '0')
  port map (
    inA_ack => ` + rf.In.Ack + `,
    inA_data => ` + rf.In.Data + `,
    inA_req => ` + rf.In.Req + `,
    outB_ack => ` + rf.Out1.Ack + `,
    outB_data => ` + rf.Out1.Data + `,
    outB_req => ` + rf.Out1.Req + `,
    outC_ack => ` + rf.Out2.Ack + `,
    outC_data=> ` + rf.Out2.Data + `,
    outC_req => ` + rf.Out2.Req + `,
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
