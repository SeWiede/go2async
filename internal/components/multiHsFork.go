package components

import (
	"errors"
	"go2async/internal/infoPrinter"
	"strconv"
)

const multiHsForkprefix = "MHSF_"

var ErrInvalidNumberOfHsComponents = errors.New("Invalid number of handshake components")

type MultiHsFork struct {
	BodyComponent

	Receivers    map[string]BodyComponentType
	ReceiverList []*MultiHsJoin
	Sender       BodyComponentType

	currentOut int

	phaseInit bool
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

		currentOut: 0,
	}, nil
}

func NewVariableMultiHsFork(sender BodyComponentType) *MultiHsFork {
	nr := multiHsForkNr
	multiHsForkNr++

	return &MultiHsFork{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch, // beware of defaultArch!
		},

		Receivers:    map[string]BodyComponentType{},
		ReceiverList: []*MultiHsJoin{},
		Sender:       sender,

		currentOut: 0,
	}
}

func (m *MultiHsFork) AddReceiver(recv *MultiHsJoin) {
	if _, ok := m.Receivers[recv.Name()]; ok {
		return
	}

	m.Receivers[recv.Name()] = recv
	m.ReceiverList = append(m.ReceiverList, recv)

	infoPrinter.DebugPrintfln("[%s]: adding receiver %s now len %d", m.Name(), recv.Name(), len(m.ReceiverList))
}

func (m *MultiHsFork) getJoinHsPos(partnerJoin *MultiHsJoin) int {
	for i, f := range partnerJoin.SenderList {
		if f.Name() == m.Name() {
			return i
		}
	}

	panic("invalid partnerJoin")
}

func (m *MultiHsFork) Name() string {
	return multiHsForkprefix + strconv.Itoa(m.number)
}

func (m *MultiHsFork) GetNumReceivers() int {
	if m.Receivers == nil && m.ReceiverList == nil {
		panic("invalid receivers")
	} else if m.ReceiverList == nil {
		return len(m.ReceiverList)
	} else {
		return len(m.Receivers)

	}
}

func (m *MultiHsFork) ComponentStr() string {
	name := m.Name()

	phase_init := "'0'"
	if m.phaseInit {
		phase_init = "'1'"
	}

	return name + `: entity work.multiHsFork
  generic map (
    HANDSHAKE_COMPONENTS => ` + strconv.Itoa(m.GetNumReceivers()) + `,
    PHASE_INIT => ` + phase_init + `
  )
  port map (
	  in_req => ` + name + `_in_req,
	  in_ack => ` + name + `_in_ack,
	  
	  out_req => ` + name + `_out_req,
	  out_ack => ` + name + `_out_ack,

	  rst => rst
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

func (m *MultiHsFork) GetSignalDefs() string {
	signalDefs := ""

	signalDefs += "signal " + m.Name() + "_in_req : std_logic;"
	signalDefs += "signal " + m.Name() + "_out_req : std_logic_vector(" + strconv.Itoa(m.GetNumReceivers()) + " - 1 downto 0);"

	signalDefs += "signal " + m.Name() + "_in_ack : std_logic;"
	signalDefs += "signal " + m.Name() + "_out_ack : std_logic_vector(" + strconv.Itoa(m.GetNumReceivers()) + " - 1 downto 0);"

	return signalDefs
}

func (m *MultiHsFork) Connect(bc BodyComponentType, x interface{}) {
	panic("not implemented")
}
