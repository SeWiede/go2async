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
	DataWidth  string

	In  *HandshakeChannel
	Out *HandshakeChannel
}

var regNr = 0

func NewReg(dataWidth string, phaseOut bool, startValue string) *Reg {
	nr := regNr
	regNr++

	name := strings.ToLower(regPrefix + strconv.Itoa(nr))
	if dataWidth == "" {
		dataWidth = "DATA_WIDTH"
	}
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
			Req:  name + "_o_req",
			Ack:  name + "_o_ack",
			Data: name + "_data",
			Out:  true,
		},
	}
}

func (r *Reg) InChannel() *HandshakeChannel {
	return r.In
}

func (r *Reg) OutChannel() *HandshakeChannel {
	return r.Out
}

func (r *Reg) Component() string {
	name := regPrefix + strconv.Itoa(r.Nr)
	phaseOutString := "PHASE_INIT_OUT => '0'"
	if r.PhaseOut {
		phaseOutString = "PHASE_INIT_OUT => '1'"
	}
	return name + `: entity work.decoupled_hs_reg(` + r.archName + `)
  generic map (
    DATA_WIDTH => ` + r.DataWidth + `,
    VALUE => ` + r.StartValue + `,
    PHASE_INIT_IN => '0',
    ` + phaseOutString + `
  )
  port map (
    rst => rst,
    in_ack => ` + r.In.Ack + `,
    in_req => ` + r.In.Req + `,
    in_data => ` + r.In.Data + `,
    -- Output channel
    out_req => ` + r.Out.Req + `,
    out_data => ` + r.Out.Data + `,
    out_ack => ` + r.Out.Ack + `
  );
  `
}

func (r *Reg) Architecture() string {
	dataSigAsgmt := "in_data"

	return `architecture ` + r.archName + ` of decoupled_hs_reg is
  signal phase_in, phase_out : std_logic;
  signal data_sig: std_logic_vector(OUT_DATA_WIDTH-1 downto 0);
  signal click : std_logic;
  
  attribute dont_touch : string;
  attribute dont_touch of  phase_in, phase_out : signal is "true";   
  attribute dont_touch of  data_sig : signal is "true";  
  attribute dont_touch of  click : signal is "true";  

begin
  out_req <= phase_out;
  in_ack <= phase_in;
  out_data <= data_sig;
  
  clock_regs: process(click, rst)
  begin
    if rst = '1' then
      phase_in <= PHASE_INIT_IN;
      phase_out <= PHASE_INIT_OUT;
      data_sig <= std_logic_vector(to_unsigned(VALUE, OUT_DATA_WIDTH));
    elsif rising_edge(click) then
      phase_in <= not phase_in after REG_CQ_DELAY;
      phase_out <= not phase_out after REG_CQ_DELAY;
      data_sig <= ` + dataSigAsgmt + ` after REG_CQ_DELAY;
    end if;
  end process;
  
  click <= (in_req xor phase_in) and (out_ack xnor phase_out) after AND2_DELAY + XOR_DELAY;

end ` + r.archName + `;`
}

func (r *Reg) ArchName() string {
	return r.archName
}
