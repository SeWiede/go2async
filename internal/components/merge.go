package components

import (
	"strconv"
	"strings"
)

const mergePrefix = "ME_"

type Merge struct {
	BodyComponent

	DataWidth int

	In1 *HandshakeChannel
	In2 *HandshakeChannel
	Out *HandshakeChannel
}

var mergeNr = 0

func NewMerge(dataWitdth int) *Merge {
	nr := mergeNr
	mergeNr++

	return &Merge{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch,
		},

		DataWidth: dataWitdth,
		/* In1: &HandshakeChannel{
			Out: false,
		},
		In2: &HandshakeChannel{
			Out: false,
		},
		Out: &HandshakeChannel{
			Req:  name + "_c_o_req",
			Ack:  name + "_c_o_ack",
			Data: name + "_c_data",
			Out:  true,
		}, */
	}
}

func (d *Merge) Name() string {
	return strings.ToLower(mergePrefix + strconv.Itoa(d.number))
}

func (d *Merge) ComponentStr() string {
	return d.Name() + `: entity work.merge
  generic map (
    DATA_WIDTH => ` + strconv.Itoa(d.DataWidth) + `
  )
  port map (
    inA_req => ` + d.Name() + `_inA_req,
    inA_ack => ` + d.Name() + `_inA_ack,
    inA_data => ` + d.Name() + `_inA_data,

    inB_req => ` + d.Name() + `_inB_req,
    inB_ack => ` + d.Name() + `_inB_ack,
    inB_data => ` + d.Name() + `_inB_data,
    
    outC_req => ` + d.Name() + `_outC_req,
    outC_ack => ` + d.Name() + `_outC_ack,
    outC_data => ` + d.Name() + `_outC_data,
    
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
