package components

import (
	"strconv"
	"strings"
)

const demuxPrefix = "DX_"

type DEMUX struct {
	BodyComponent

	DataWidth int

	In     *HandshakeChannel
	Out1   *HandshakeChannel
	Out2   *HandshakeChannel
	Select *HandshakeChannel
}

var demuxNr = 0

func NewDEMUX(dataWidth int) *DEMUX {
	nr := demuxNr
	demuxNr++

	return &DEMUX{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch,
		},

		DataWidth: dataWidth,

		/*In: &HandshakeChannel{
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
		}, */
	}
}

func (d *DEMUX) Name() string {
	return strings.ToLower(demuxPrefix + strconv.Itoa(d.number))
}

func (d *DEMUX) ComponentStr() string {
	return d.Name() + `: entity work.demux
  generic map (
    DATA_WIDTH => ` + strconv.Itoa(d.DataWidth) + `
  )
  port map (
    inA_req => ` + d.Name() + `_inA_req,
    inA_ack => ` + d.Name() + `_inA_ack,
    inA_data => ` + d.Name() + `_inA_data,

    outB_req => ` + d.Name() + `_outB_req,
    outB_ack => ` + d.Name() + `_outB_ack,
    outB_data => ` + d.Name() + `_outB_data,
    
    outC_req => ` + d.Name() + `_outC_req,
    outC_ack => ` + d.Name() + `_outC_ack,
    outC_data => ` + d.Name() + `_outC_data,

    inSel_req => ` + d.Name() + `_inSel_req,
    inSel_ack => ` + d.Name() + `_inSel_ack,
    selector => ` + d.Name() + `_selector,
    
    rst => rst
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

func (d *DEMUX) Entity() string {
	panic("demux is predefined")
}

func (d *DEMUX) EntityName() string {
	return "demux"
}

func (d *DEMUX) GetSignalDefs() string {
	panic("signaldefs not implemented")
	return ""
}

func (d *DEMUX) Connect(bc BodyComponentType, x interface{}) {
	panic("not implemented")
}
