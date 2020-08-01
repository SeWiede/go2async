package components

import (
	"strconv"
	"strings"
)

const muxprefix = "MX_"

type MUX struct {
	Nr       int
	archName string

	In1    *HandshakeChannel
	In2    *HandshakeChannel
	Out    *HandshakeChannel
	Select *HandshakeChannel
}

var muxNr = 0

func NewMUX() *MUX {
	nr := muxNr
	muxNr++

	name := strings.ToLower(muxprefix + strconv.Itoa(nr))
	return &MUX{
		Nr:       nr,
		archName: defaultArch,
		In1: &HandshakeChannel{
			Out: false,
		},
		In2: &HandshakeChannel{
			Out: false,
		},
		Out: &HandshakeChannel{
			Req:  name + "_o_req",
			Ack:  name + "_o_ack",
			Data: name + "_data",
			Out:  true,
		},
		Select: &HandshakeChannel{
			Out: false,
		},
	}
}

func (m *MUX) Component() string {
	name := muxprefix + strconv.Itoa(m.Nr)
	return name + `: entity work.mux
  generic map (
    DATA_WIDTH => DATA_WIDTH
  )
  port map (
    inA_ack => ` + m.In1.Ack + `,
    inA_data => ` + m.In1.Data + `,
    inA_req => ` + m.In1.Req + `,
    inB_ack => ` + m.In2.Ack + `,
    inB_data => ` + m.In2.Data + `,
    inB_req => ` + m.In2.Req + `,
    outC_ack => ` + m.Out.Ack + `,
    outC_data => ` + m.Out.Data + `,
    outC_req => ` + m.Out.Req + `,
    rst => rst,
    inSel_ack => ` + m.Select.Ack + `,
    inSel_req => ` + m.Select.Req + `,
    selector => ` + m.Select.Data + `
  );
  `
}

func (m *MUX) Architecture() string {
	return `architecture ` + m.archName + ` of mux is
  -- the registers
  signal phase_c, phase_sel, inSel_token : std_logic;
  -- register control
  signal phase_a : std_logic;
  signal phase_b : std_logic;
  -- Clock
  signal click_req, click_ack : std_logic;
  signal pulse : std_logic;
  -- control gates
  signal inA_token, inB_token : std_logic;
  signal selected_a, selected_b : std_logic;
  
  attribute dont_touch : string;
  attribute dont_touch of  phase_sel, phase_c, phase_a, phase_b : signal is "true";   
  attribute dont_touch of  click_req, click_ack : signal is "true";  

begin
  -- Control Path
  inSel_ack <= phase_sel;
  outC_req <= phase_c;
  outC_data <= inA_data when selector(0) = '1' else inB_data;
  inA_ack <= phase_a;
  inB_ack <= phase_b;
  
  --input state
  inA_token <= phase_a xor inA_req after XOR_DELAY;
  inB_token <= phase_b xor inB_req after XOR_DELAY;
  inSel_token <= phase_sel xor inSel_req after XOR_DELAY;
  
  --Selector triggered pulse
  click_req <= (inA_token and inSel_token and selector(0)) or (inB_token and inSel_token and not selector(0)) after AND2_DELAY + OR2_DELAY;
  
  --Output state
  click_ack <= phase_c xnor outC_ack after XOR_DELAY;
  
  req_regs : process(click_req, rst)
    begin
      if rst = '1' then
        phase_c <= PHASE_INIT_C;
      elsif rising_edge(click_req) then
        -- Click control register loops back to itself
        phase_c <= not phase_c after REG_CQ_DELAY;
      end if;
    end process;
    
  ack_regs : process(click_ack, rst)
    begin
      if rst = '1' then
        phase_a <= PHASE_INIT_A;
        phase_b <= PHASE_INIT_B;
        phase_sel <= PHASE_INIT_SEL;
      elsif rising_edge(click_ack) then
        phase_a <= phase_a xor selector(0) after REG_CQ_DELAY;
        phase_b <= phase_b xor not(selector(0)) after REG_CQ_DELAY;
        phase_sel <= inSel_req after REG_CQ_DELAY;
      end if;
    end process;

end ` + m.archName + `;
`
}

func (m *MUX) ArchName() string {
	return m.archName
}
