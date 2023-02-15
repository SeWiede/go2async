package components

import (
	"strconv"
	"strings"
)

const regPrefix = "R_"

type Reg struct {
	BodyComponent

	PhaseOut bool

	StartValue string
}

var regNr = 0

func NewReg(parent BlockType, phaseOut bool, startValue string) *Reg {
	nr := regNr
	regNr++

	// name := strings.ToLower(regPrefix + strconv.Itoa(nr))

	newReg := &Reg{
		BodyComponent: BodyComponent{
			name:     strings.ToLower(regPrefix + strconv.Itoa(nr)),
			archName: defaultArch,
			number:   nr,

			parentBlock: parent,

			inputVariables: parent.OutputVariables(),
		},
		PhaseOut:   phaseOut,
		StartValue: startValue,
	}

	newReg.In = append(newReg.In, NewInputHandshakeChannel(newReg, newReg.Name()+"_in_req", newReg.Name()+"_in_ack"))
	newReg.Out = append(newReg.Out, NewOutputHandshakeChannel(newReg, newReg.Name()+"_out_req", newReg.Name()+"_out_ack"))

	newReg.InData = append(newReg.InData, NewInDataChannel(newReg, newReg.InputVariables(), newReg.Name()+"_in_data"))
	newReg.OutData = append(newReg.OutData, NewOutDataChannel(newReg, newReg.InputVariables(), newReg.Name()+"_out_data"))

	return newReg
}

func (r *Reg) Name() string {
	return r.name
}

func (r *Reg) ComponentStr() string {
	phaseOutString := "PHASE_INIT_OUT => '0'"
	if r.PhaseOut {
		phaseOutString = "PHASE_INIT_OUT => '1'"
	}

	return r.Name() + `: entity work.decoupled_hs_reg(` + r.archName + `)
  generic map (
    DATA_WIDTH => ` + strconv.Itoa(r.InputVariables().Size) + `,
    VALUE => ` + r.StartValue + `,
    PHASE_INIT_IN => '0',
    ` + phaseOutString + `
  )
  port map (
    -- Input channel
    in_req => ` + r.In[0].GetReqSignalName() + `,
    in_ack => ` + r.In[0].GetAckSignalName() + `,
    in_data => ` + r.InData[0].GetDataSignalName() + `,

    -- Output Channel 1
    out_req => ` + r.Out[0].GetReqSignalName() + `,
    out_ack => ` + r.Out[0].GetAckSignalName() + `,
    out_data => ` + r.OutData[0].GetDataSignalName() + `,

    rst => rst
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

func (r *Reg) Entity() string {
	panic("reg is predefined")
}

func (r *Reg) EntityName() string {
	return "Reg"
}
