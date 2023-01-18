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
	panic("MultiHsFork has predefined architecture")
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
