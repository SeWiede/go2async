package components

import (
	"go2async/internal/variable"
	"strconv"
	"strings"
)

const demuxPrefix = "DX_"

type Demux struct {
	BodyComponent

	/*
		 	In     *HandshakeChannel
			Out1   *HandshakeChannel
			Out2   *HandshakeChannel
			Select *HandshakeChannel
	*/
}

var demuxNr = 0

func NewDemux(parent BlockType) *Demux {
	nr := demuxNr
	demuxNr++

	newDemux := &Demux{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch,
			name:     strings.ToLower(demuxPrefix + strconv.Itoa(nr)),

			parentBlock: parent,

			inputVariables: parent.InputVariables(),
		},
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

	newDemux.In = append(newDemux.In, NewInputHandshakeChannel(newDemux, newDemux.Name()+"_inA_req", newDemux.Name()+"_inA_ack"))
	newDemux.Out = append(newDemux.Out, NewOutputHandshakeChannel(newDemux, newDemux.Name()+"_outB_req", newDemux.Name()+"_outB_ack"))
	newDemux.Out = append(newDemux.Out, NewOutputHandshakeChannel(newDemux, newDemux.Name()+"_outC_req", newDemux.Name()+"_outC_ack"))

	newDemux.InData = append(newDemux.InData, NewInDataChannel(newDemux, newDemux.InputVariables(), newDemux.Name()+"_inA_data"))
	newDemux.OutData = append(newDemux.OutData, NewOutDataChannel(newDemux, newDemux.InputVariables(), newDemux.Name()+"_outB_data"))
	newDemux.OutData = append(newDemux.OutData, NewOutDataChannel(newDemux, newDemux.InputVariables(), newDemux.Name()+"_outC_data"))

	// Selector
	newDemux.In = append(newDemux.In, NewInputHandshakeChannel(newDemux, newDemux.Name()+"_inSel_req", newDemux.Name()+"_inSel_ack"))

	selectorVariables := NewScopedVariables()
	selectorVariables.AddVariable(&variable.VariableTypeDecl{
		Name_: "__go2async_selector__var",
		Typ_:  "__go2async_selector",
		Len_:  1,
	})
	newDemux.InData = append(newDemux.InData, NewInDataChannel(newDemux, selectorVariables, newDemux.Name()+"_selector"))

	return newDemux
}

func (d *Demux) Name() string {
	return d.name
}

func (d *Demux) ComponentStr() string {
	return d.Name() + `: entity work.demux
  generic map (
    DATA_WIDTH => ` + strconv.Itoa(d.inputVariables.Size) + `
  )
  port map (
    inA_req => ` + d.In[0].GetReqSignalName() + `,
    inA_ack => ` + d.In[0].GetAckSignalName() + `,
    inA_data => ` + d.InData[0].GetDataSignalName() + `,

    outB_req => ` + d.Out[0].GetReqSignalName() + `,
    outB_ack => ` + d.Out[0].GetAckSignalName() + `,
    outB_data => ` + d.OutData[0].GetDataSignalName() + `,
    
    outC_req => ` + d.Out[1].GetReqSignalName() + `,
    outC_ack => ` + d.Out[1].GetAckSignalName() + `,
    outC_data => ` + d.OutData[1].GetDataSignalName() + `,

    inSel_req => ` + d.In[1].GetReqSignalName() + `,
    inSel_ack => ` + d.In[1].GetAckSignalName() + `,
    selector => ` + d.InData[1].GetDataSignalName() + `,
    
    rst => rst
  );
   `
}

func (d *Demux) Architecture() string {
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

func (d *Demux) ArchName() string {
	return d.archName
}

func (d *Demux) Entity() string {
	panic("demux is predefined")
}

func (d *Demux) EntityName() string {
	return "demux"
}
