package components

import (
	"errors"
	"strconv"
)

const multiHsForkprefix = "MHSF_"

var ErrInvalidNumberOfHsComponents = errors.New("Invalid number of handshake components")

type MultiHsFork struct {
	BodyComponent

	Receivers map[string]BodyComponentType
	Sender    BodyComponentType

	NumHsComponents int
}

var multiHsForkNr = 0

func NewMultiHsFork(receivers map[string]BodyComponentType, sender BodyComponentType) (*MultiHsFork, error) {
	comps := len(receivers)
	if comps <= 1 {
		return nil, ErrInvalidNumberOfHsComponents
	}

	nr := multiHsForkNr
	multiHsForkNr++

	return &MultiHsFork{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch, // beware of defaultArch!
		},

		Receivers: receivers,
		Sender:    sender,

		NumHsComponents: comps,
	}, nil
}

func (m *MultiHsFork) Name() string {
	return multiHsForkprefix + strconv.Itoa(m.number)
}

func (m *MultiHsFork) ComponentStr() string {
	name := m.Name()

	return name + `: entity work.multiHsFork
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

func (m *MultiHsFork) Architecture() string {
	// TODO: delays (click)

	// -- click <= (outC_ack AND outB_ack AND NOT(phase)) OR (NOT(outC_ack) AND NOT(outB_ack) AND phase) AFTER AND3_DELAY + OR2_DELAY;

	return `ARCHITECTURE ` + m.archName + ` OF multiHsFork IS

  SIGNAL click : STD_LOGIC;
  SIGNAL phase : STD_LOGIC := PHASE_INIT;

  ATTRIBUTE dont_touch : STRING;
  ATTRIBUTE dont_touch OF phase : SIGNAL IS "true";
  ATTRIBUTE dont_touch OF click : SIGNAL IS "true";

  SIGNAL out_ack_and : STD_LOGIC_VECTOR(out_ack'length - 1 downto 0) := (others => '0');
  SIGNAL out_ack_nand : STD_LOGIC_VECTOR(out_ack'length - 1 downto 0) := (others => '0');
BEGIN
  -- Control Path
  out_req <= (others => in_req);

  in_ack <= phase;

  
  out_ack: for i in 0 to out_ack'length - 1 generate
    out_ack_and <= out_ack(i) AND out_ack(i+1);
  end generate;
  
  out_ack_n: for i in 0 to out_ack'length - 1 generate
    out_ack_nand <= NOT(out_ack(i)) AND NOT(out_ack(i+1));
  end generate;
  
  click <= (out_ack_and AND NOT(phase)) OR (out_ack_nand AND phase) AFTER AND3_DELAY + OR2_DELAY;
  
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

  end ` + m.archName + `;
`
}

func (m *MultiHsFork) EntityName() string {
	return "multiHsFork"
}

func (m *MultiHsFork) Entity() string {
	return `LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.click_element_library_constants.ALL;

ENTITY MultiHsFork IS
  GENERIC (
    HANDSHAKE_COMPONENTS : NATURAL := 2;
    PHASE_INIT : STD_LOGIC := '0'
  PORT (
    rst : IN STD_LOGIC;
    --Input handshakes
    in_req : IN STD_LOGIC;
    in_ack : OUT STD_LOGIC;

    --Output handshakes
    out_req : OUT STD_LOGIC_VECTOR(HANDSHAKE_COMPONENTS - 1 DOWNTO 0);
    out_ack : IN STD_LOGIC_VECTOR(HANDSHAKE_COMPONENTS - 1 DOWNTO 0);
  );
END MultiHsFork;`
}
