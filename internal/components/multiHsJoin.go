package components

import (
	"go2async/pkg/variable"
	"strconv"
)

const multiHsJoinprefix = "MHSJ_"

type MultiHsJoin struct {
	BodyComponent

	Senders  []BodyComponentType
	Receiver BodyComponentType

	NumHsComponents int
}

var multiHsJoinNr = 0

func NewMultiHsJoin(senders []BodyComponentType, receiver BodyComponentType) (*MultiHsJoin, error) {
	comps := len(senders)

	if comps <= 1 {
		return nil, ErrInvalidNumberOfHsComponents
	}

	nr := multiHsJoinNr
	multiHsJoinNr++

	return &MultiHsJoin{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch, // beware of defaultArch!

			inputVariables:  []*variable.VariableInfo{},
			outputVariables: []*variable.VariableInfo{},
		},

		Senders:  senders,
		Receiver: receiver,

		NumHsComponents: comps,
	}, nil
}

func (m *MultiHsJoin) Name() string {
	return multiHsJoinprefix + strconv.Itoa(m.number)
}

func (m *MultiHsJoin) ComponentStr() string {
	name := m.Name()

	return name + `: entity work.multiHsJoin
  generic map (
    HANDSHAKE_COMPONENTS => ` + strconv.Itoa(m.NumHsComponents) + `,
    PHASE_INIT => '0'
  )
  port map (
    rst => rst,

    in_req => ` + name + `_in_req,
    in_ack => ` + name + `_in_ack,

    out_req => ` + name + `_out_req,
    out_ack => ` + name + `_out_ack
  );
  `
}

func (m *MultiHsJoin) Architecture() string {
	// TODO: delays (click)

	// click <= (inA_req and inB_req and not(phase)) or (not(inA_req) and not(inB_req) and phase) after AND3_DELAY + OR2_DELAY;

	return `ARCHITECTURE ` + m.ArchName() + ` OF multiHsJoin IS

  SIGNAL click : STD_LOGIC;
  SIGNAL phase : STD_LOGIC := PHASE_INIT;

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click : SIGNAL IS "true";

  SIGNAL in_req_and : STD_LOGIC_VECTOR(in_req'length - 1 downto 0) := (others => '0');
  SIGNAL in_req_nand : STD_LOGIC_VECTOR(in_req'length - 1 downto 0) := (others => '0');
BEGIN
  -- Control Path
  out_req <= phase;

  in_ack <= (others => out_ack);

  
  in_req: for i in 0 to in_req'length - 1 generate
    in_req_and <= in_req(i) AND in_req(i+1);
  end generate;
  
  in_req_n: for i in 0 to in_req'length - 1 generate
    in_req_nand <= NOT(in_req(i)) AND NOT(in_req(i+1));
  end generate;

  click <= (in_req_and AND NOT(phase)) OR (in_req_nand AND phase) AFTER AND3_DELAY + OR2_DELAY;
  
  clock_regs : PROCESS (click, rst)
  BEGIN
    IF rst = '1' THEN
      phase <= PHASE_INIT;
    ELSE
      IF rising_edge(click) THEN
        phase <= NOT phase AFTER REG_CQ_DELAY;
      END IF;
    END IF;
  END PROCESS clock_regs;

  end ` + m.ArchName() + `;
`
}

func (m *MultiHsJoin) EntityName() string {
	return "multiHsJoin"
}

func (m *MultiHsJoin) Entity() string {
	return `LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.click_element_library_constants.ALL;

ENTITY MultiHsJoin IS
  GENERIC (
    HANDSHAKE_COMPONENTS : NATURAL := 2;
    PHASE_INIT : STD_LOGIC := '0'
  PORT (
    rst : IN STD_LOGIC;
    --Input handshakes
    in_req : IN STD_LOGIC_VECTOR(HANDSHAKE_COMPONENTS - 1 DOWNTO 0);
    in_ack : OUT STD_LOGIC_VECTOR(HANDSHAKE_COMPONENTS - 1 DOWNTO 0);

    --Output handshakes
    out_req : OUT STD_LOGIC;
    out_ack : IN STD_LOGIC;
  );
END MultiHsJoin;`
}
