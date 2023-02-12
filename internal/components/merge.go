package components

import (
	"strconv"
	"strings"
)

const mergePrefix = "ME_"

type Merge struct {
	BodyComponent
}

var mergeNr = 0

func NewMerge(parent BlockType) *Merge {
	nr := mergeNr
	mergeNr++

	newMerge := &Merge{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch,
			name:     strings.ToLower(mergePrefix + strconv.Itoa(nr)),

			parentBlock: parent,

			inputVariables: parent.InputVariables(),
		},
	}

	newMerge.In = append(newMerge.In, NewInputHandshakeChannel(newMerge, newMerge.Name()+"_inA_req", newMerge.Name()+"_inA_ack"))
	newMerge.In = append(newMerge.In, NewInputHandshakeChannel(newMerge, newMerge.Name()+"_inB_req", newMerge.Name()+"_inB_ack"))
	newMerge.Out = append(newMerge.Out, NewOutputHandshakeChannel(newMerge, newMerge.Name()+"_outC_req", newMerge.Name()+"_outC_ack"))

	newMerge.InData = append(newMerge.InData, NewInDataChannel(newMerge, newMerge.InputVariables(), newMerge.Name()+"_inA_data"))
	newMerge.InData = append(newMerge.InData, NewInDataChannel(newMerge, newMerge.InputVariables(), newMerge.Name()+"_inB_data"))
	newMerge.OutData = append(newMerge.OutData, NewOutDataChannel(newMerge, newMerge.InputVariables(), newMerge.Name()+"_outC_data"))

	return newMerge
}

func (d *Merge) Name() string {
	return d.name
}

func (d *Merge) ComponentStr() string {
	return d.Name() + `: entity work.merge
  generic map (
    DATA_WIDTH => ` + strconv.Itoa(d.inputVariables.Size) + `
  )
  port map (
    inA_req => ` + d.In[0].GetReqSignalName() + `,
    inA_ack => ` + d.In[0].GetAckSignalName() + `,
    inA_data => ` + d.InData[0].GetDataSignalName() + `,

    inB_req => ` + d.In[1].GetReqSignalName() + `,
    inB_ack => ` + d.In[1].GetAckSignalName() + `,
    inB_data => ` + d.InData[1].GetDataSignalName() + `,
    
    outC_req => ` + d.Out[0].GetReqSignalName() + `,
    outC_ack => ` + d.Out[0].GetAckSignalName() + `,
    outC_data => ` + d.OutData[0].GetDataSignalName() + `,
    
    rst => rst
   );
   `
}

func (d *Merge) Architecture() string {
	return `architecture ` + d.archName + ` of merge is

  signal inA_token, inB_token, outC_bubble : std_logic;
  signal phase_a, phase_b, phase_c: std_logic;
  signal click : std_logic;
  signal data_reg, data_sig: std_logic_vector(DATA_WIDTH-1 downto 0);
  
  attribute dont_touch : string;
  attribute dont_touch of  phase_c, phase_a, phase_b, inA_token, inB_token : signal is "true";   
  attribute dont_touch of  click : signal is "true";  

begin
  inA_token <= inA_req xor phase_a after XOR_DELAY;
  inB_token <= inB_req xor phase_b after XOR_DELAY;
  outC_bubble <= phase_c xnor outC_ack after XOR_DELAY;
  -- Click function
  click <= inA_token or inB_token after OR2_DELAY;

  clock_req : process(click, rst)
    begin
      if rst = '1' then
        phase_c <= PHASE_INIT_C;
      elsif rising_edge(click) then
        phase_c <= not phase_c after REG_CQ_DELAY;
      end if;
    end process;
    
    
  clock_ack : process(outC_bubble, rst)
    begin
      if rst = '1' then
        phase_a <= PHASE_INIT_A;
        phase_b <= PHASE_INIT_B;
      elsif rising_edge(outC_bubble) then
        phase_a <= inA_req after REG_CQ_DELAY;
        phase_b <= inB_req after REG_CQ_DELAY;
      end if;
    end process;
    
  outC_data <= inA_data when inA_token = '1' else 
               inB_data when inB_token = '1' else 
               (others => '0');
  outC_req <= phase_c;
  inA_ack <= phase_a;
  inB_ack <= phase_b;

end ` + d.archName + `;`
}

func (d *Merge) ArchName() string {
	return d.archName
}

func (d *Merge) Entity() string {
	panic("merge is predefined")
}

func (d *Merge) EntityName() string {
	return "merge"
}
