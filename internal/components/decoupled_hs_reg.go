package components

import (
	"strconv"
	"strings"
)

const regPrefix = "R_"

type Reg struct {
	Nr       int
	archName string
	PhaseOut bool

	StartValue string
	DataWidth  int

	In  *HandshakeChannel
	Out *HandshakeChannel
}

var regNr = 0

func NewReg(dataWidth int, phaseOut bool, startValue string) *Reg {
	nr := regNr
	regNr++

	name := strings.ToLower(regPrefix + strconv.Itoa(nr))

	return &Reg{
		Nr:         nr,
		archName:   defaultArch,
		DataWidth:  dataWidth,
		PhaseOut:   phaseOut,
		StartValue: startValue,
		In: &HandshakeChannel{
			Out: false,
		},
		Out: &HandshakeChannel{
			Req:       name + "_o_req",
			Ack:       name + "_o_ack",
			Data:      name + "_data",
			Out:       true,
			DataWidth: dataWidth,
		},
	}
}

func (r *Reg) InChannel() *HandshakeChannel {
	return r.In
}

func (r *Reg) OutChannel() *HandshakeChannel {
	return r.Out
}

func (r *Reg) Name() string {
	return regPrefix + strconv.Itoa(r.Nr)
}

func (r *Reg) ComponentStr() string {
	phaseOutString := "PHASE_INIT_OUT => '0'"
	if r.PhaseOut {
		phaseOutString = "PHASE_INIT_OUT => '1'"
	}

	dataWidthStr := "DATA_WIDTH"
	if r.DataWidth > 0 {
		dataWidthStr = strconv.Itoa(r.DataWidth)
	}

	return r.Name() + `: entity work.decoupled_hs_reg(` + r.archName + `)
  generic map (
    DATA_WIDTH => ` + dataWidthStr + `,
    VALUE => ` + r.StartValue + `,
    PHASE_INIT_IN => '0',
    ` + phaseOutString + `
  )
  port map (
    rst => rst,
    -- Input channel
    in_req => ` + r.Name() + `_in_req,
    in_ack => ` + r.Name() + `_in_ack,
    in_data => ` + r.Name() + `_in_data,
    -- Output channel
    out_req => ` + r.Name() + `_out_req,
    out_ack => ` + r.Name() + `_out_ack,
    out_data => ` + r.Name() + `_out_data
  );
  `
}

func (r *Reg) Architecture() string {
	dataSigAsgmt := "in_data"

	return `architecture ` + r.archName + ` of decoupled_hs_reg is
  signal phase_in, phase_out, in_req_d, out_ack_d : std_logic;
  signal data_sig: std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal click : std_logic;
  
  attribute dont_touch : string;
  attribute dont_touch of  phase_in, phase_out : signal is "true";   
  attribute dont_touch of  data_sig : signal is "true";  
  attribute dont_touch of  click : signal is "true";  
  attribute dont_touch of  in_req_d, out_ack_d : signal is "true";  

begin
  out_req <= phase_out;
  in_ack <= phase_in;
  out_data <= data_sig;
  
  clock_regs: process(click, rst)
  begin
    if rst = '1' then
      phase_in <= PHASE_INIT_IN;
      phase_out <= PHASE_INIT_OUT;
      data_sig <= std_logic_vector(to_unsigned(VALUE, DATA_WIDTH));
    elsif rising_edge(click) then
      phase_in <= not phase_in after REG_CQ_DELAY;
      phase_out <= not phase_out after REG_CQ_DELAY;
      data_sig <= ` + dataSigAsgmt + ` after REG_CQ_DELAY;
    end if;
  end process;

  delay_req: entity work.delay_element
      generic map(
        NUM_LCELLS => 16  -- Delay  size
      )
      port map (
        i => in_req,
        o => in_req_d
	  );

	delay_ack: entity work.delay_element
      generic map(
        NUM_LCELLS => 16  -- Delay  size
      )
      port map (
        i => out_ack,
        o => out_ack_d
	  );
  
  click <= (in_req_d xor phase_in) and (out_ack_d xnor phase_out) after AND2_DELAY + XOR_DELAY;

end ` + r.archName + `;`
}

func (r *Reg) ArchName() string {
	return r.archName
}
