package components

import (
	"strconv"
)

const multiHsJoinprefix = "MHSJ_"

type MultiHsJoin struct {
	BodyComponent

	Senders    map[string]BodyComponentType
	SenderList []*MultiHsFork
	Receiver   BodyComponentType

	currentIn int

	phaseInit bool
}

var multiHsJoinNr = 0

func NewMultiHsJoin(senders map[string]BodyComponentType, receiver BodyComponentType) (*MultiHsJoin, error) {
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
		},

		Senders:  senders,
		Receiver: receiver,

		currentIn: 0,
	}, nil
}

func NewVariableMultiHsJoin(Receiver BodyComponentType) *MultiHsJoin {
	nr := multiHsForkNr
	multiHsForkNr++

	return &MultiHsJoin{
		BodyComponent: BodyComponent{
			number:   nr,
			archName: defaultArch, // beware of defaultArch!
		},

		Senders:    map[string]BodyComponentType{},
		SenderList: []*MultiHsFork{},
		Receiver:   Receiver,

		currentIn: 0,
	}
}

func (m *MultiHsJoin) AddSender(snd *MultiHsFork) {
	if _, ok := m.Senders[snd.Name()]; ok {
		return
	}

	m.Senders[snd.Name()] = snd
	m.SenderList = append(m.SenderList, snd)
}

func (m *MultiHsJoin) getForkHsPos(partnerJoin *MultiHsFork) int {
	for i, j := range partnerJoin.ReceiverList {
		if j.Name() == m.Name() {
			return i
		}
	}

	panic("invalid partnerJoin")
}

func (m *MultiHsJoin) Name() string {
	return multiHsJoinprefix + strconv.Itoa(m.number)
}

func (m *MultiHsJoin) GetNumSenders() int {
	if m.Senders == nil && m.SenderList == nil {
		panic("invalid receivers")
	} else if m.SenderList == nil {
		return len(m.SenderList)
	} else {
		return len(m.Senders)

	}
}

func (m *MultiHsJoin) ComponentStr() string {
	name := m.Name()

	phase_init := "'0'"
	if m.phaseInit {
		phase_init = "'1'"
	}

	return name + `: entity work.multiHsJoin
  generic map (
    HANDSHAKE_COMPONENTS => ` + strconv.Itoa(m.GetNumSenders()) + `,
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

func (m *MultiHsJoin) Architecture() string {
	panic("multiJSJoin has predefined architecture")
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

func (m *MultiHsJoin) GetSignalDefs() string {
	signalDefs := ""

	signalDefs += "signal " + m.Name() + "_in_req : std_logic_vector(" + strconv.Itoa(m.GetNumSenders()) + " - 1 downto 0);"
	signalDefs += "signal " + m.Name() + "_out_req : std_logic;"

	signalDefs += "signal " + m.Name() + "_in_ack : std_logic_vector(" + strconv.Itoa(m.GetNumSenders()) + " - 1 downto 0);"
	signalDefs += "signal " + m.Name() + "_out_ack : std_logic;"

	return signalDefs
}

func (m *MultiHsJoin) Connect(bc BodyComponentType, x interface{}) {
	panic("not implemented")
}
