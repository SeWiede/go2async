package components

import (
	"go2async/internal/variable"
	"strconv"
	"strings"
)

const muxprefix = "MX_"

type MUX struct {
	BodyComponent
}

var muxNr = 0

func NewMUX(parent BlockType) *MUX {
	nr := muxNr
	muxNr++

	newMux := &MUX{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch,
			name:     strings.ToLower(muxprefix + strconv.Itoa(nr)),

			parentBlock: parent,

			inputVariables: parent.InputVariables(),
		},
	}

	newMux.In = append(newMux.In, NewInputHandshakeChannel(newMux, newMux.Name()+"_inA_req", newMux.Name()+"_inA_ack"))
	newMux.In = append(newMux.In, NewInputHandshakeChannel(newMux, newMux.Name()+"_inB_req", newMux.Name()+"_inB_ack"))
	newMux.Out = append(newMux.Out, NewOutputHandshakeChannel(newMux, newMux.Name()+"_outC_req", newMux.Name()+"_outC_ack"))

	newMux.InData = append(newMux.InData, NewInDataChannel(newMux, newMux.InputVariables(), newMux.Name()+"_inA_data"))
	newMux.InData = append(newMux.InData, NewInDataChannel(newMux, newMux.InputVariables(), newMux.Name()+"_inB_data"))
	newMux.OutData = append(newMux.OutData, NewOutDataChannel(newMux, newMux.InputVariables(), newMux.Name()+"_outC_data"))

	// Selector
	newMux.In = append(newMux.In, NewInputHandshakeChannel(newMux, newMux.Name()+"_inSel_req", newMux.Name()+"_inSel_ack"))

	selectorVariables := NewScopedVariables()
	selectorVariables.AddVariable(&variable.VariableTypeDecl{
		Name_: "__go2async_selector__var",
		Typ_:  "__go2async_selector",
		Len_:  1,
	})
	newMux.InData = append(newMux.InData, NewInDataChannel(newMux, selectorVariables, newMux.Name()+"_selector"))

	return newMux
}

func (m *MUX) Name() string {
	return m.name
}

func (m *MUX) ComponentStr() string {
	return m.Name() + `: entity work.mux
  generic map (
    DATA_WIDTH => ` + strconv.Itoa(m.inputVariables.Size) + `
  )
  port map (
    inA_req => ` + m.In[0].GetReqSignalName() + `,
    inA_ack => ` + m.In[0].GetAckSignalName() + `,
    inA_data => ` + m.InData[0].GetDataSignalName() + `,

    inB_req => ` + m.In[1].GetReqSignalName() + `,
    inB_ack => ` + m.In[1].GetAckSignalName() + `,
    inB_data => ` + m.InData[1].GetDataSignalName() + `,
    
    outC_req => ` + m.Out[0].GetReqSignalName() + `,
    outC_ack => ` + m.Out[0].GetAckSignalName() + `,
    outC_data => ` + m.OutData[0].GetDataSignalName() + `,

    inSel_req => ` + m.In[2].GetReqSignalName() + `,
    inSel_ack => ` + m.In[2].GetAckSignalName() + `,
    selector => ` + m.InData[2].GetDataSignalName() + `,
    
    rst => rst
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

func (m *MUX) Entity() string {
	panic("mux is predefined")
}

func (m *MUX) EntityName() string {
	return "demux"
}
