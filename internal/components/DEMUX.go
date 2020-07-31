package components

import (
	"strconv"
	"strings"
)

const demuxPrefix = "DX_"

type DEMUX struct {
	Nr       int
	archName string

	In     *HandshakeChannel
	Out1   *HandshakeChannel
	Out2   *HandshakeChannel
	Select *HandshakeChannel
}

var demuxNr = 0

func NewDEMUX() *DEMUX {
	nr := demuxNr
	demuxNr++

	name := strings.ToLower(demuxPrefix + strconv.Itoa(nr))
	demuxNr++
	return &DEMUX{
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
		Select: &HandshakeChannel{
			Out: false,
		},
	}
}

func (d *DEMUX) Component() string {
	name := demuxPrefix + strconv.Itoa(d.Nr)
	return name + `: entity work.demux
  generic map (
    VARIABLE_WIDTH => VARIABLE_WIDTH,
    DATA_MULTIPLIER => DATA_MULTIPLIER,
    DATA_WIDTH => DATA_WIDTH
  )
  port map (
    inA_ack => ` + d.In.Ack + `,
    inA_data => ` + d.In.Data + `,
    inA_req => ` + d.In.Req + `,
    outB_ack => ` + d.Out1.Ack + `,
    outB_data => ` + d.Out1.Data + `,
    outB_req => ` + d.Out1.Req + `,
    outC_ack => ` + d.Out2.Ack + `,
    outC_data => ` + d.Out2.Data + `,
    outC_req => ` + d.Out2.Req + `,
    rst => rst,
    inSel_ack => ` + d.Select.Ack + `,
    inSel_req => ` + d.Select.Req + `,
    selector => ` + d.Select.Data + `
  );
   `
}

func (d *DEMUX) Architecture() string {
	return `architecture ` + d.archName + ` of demux is

  signal phase_a : std_logic;
  signal click_req, click_ack : std_logic;
  
  signal phase_b : std_logic;
  signal phase_c : std_logic;

begin
    
  -- Control Path   
  inSel_ack <= phase_a;
  inA_ack <= phase_a;
  outB_req <= phase_b;
  outB_data <= inA_data;
  outC_req <= phase_c;
  outC_data <= inA_data;
  
  -- Request FF clock function
  click_req <= (inSel_req and not(phase_a) and inA_req) or (not(inSel_req) and phase_a and not(inA_req)) after ANDOR3_DELAY + NOT1_DELAY;
  
  -- Acknowledge FF clock function
  click_ack <= (outB_ack xnor phase_b) and (outC_ack xnor phase_c) after AND2_DELAY + XOR_DELAY + NOT1_DELAY;

  req : process(click_req, rst)
    begin
      if rst = '1' then
        phase_b <= PHASE_INIT_B;
        phase_c <= PHASE_INIT_C;
      elsif rising_edge(click_req) then
        phase_b <= phase_b xor selector(0) after REG_CQ_DELAY;
        phase_c <= phase_c xor not(selector(0)) after REG_CQ_DELAY;
      end if;
    end process req;
    
  ack : process(click_ack, rst)
    begin
      if rst = '1' then
        phase_a <= PHASE_INIT_A;
      elsif rising_edge(click_ack) then
        phase_a <= not phase_a after REG_CQ_DELAY;
      end if;
    end process ack;
	
end ` + d.archName + `;
`
}

func (d *DEMUX) ArchName() string {
	return d.archName
}
