package components

import (
	"errors"
	"strconv"
)

type HandshakeChannel struct {
	Req  string
	Ack  string
	Data string

	DataWidth int

	Out bool
}

var DummyOutChannel = &HandshakeChannel{
	Req:  "DUMMY_REQ",
	Ack:  "DUMMY_ACK",
	Data: "DUMMY_DATA",
	Out:  true,
}

var DummyInChannel = &HandshakeChannel{
	Req:  "DUMMY_REQ",
	Ack:  "DUMMY_ACK",
	Data: "DUMMY_DATA",
	Out:  false,
}

func (hw *HandshakeChannel) Connect(to *HandshakeChannel) (*HandshakeChannel, error) {
	if to == nil {
		return nil, errors.New("To channel was nil")
	}
	if hw.Out == to.Out {
		return nil, errors.New("Cannot connect two channels with the same direction.")
	}
	if hw == DummyInChannel || hw == DummyOutChannel || to == DummyInChannel || to == DummyOutChannel {
		return nil, errors.New("Cannot connect dummy channels")
	}

	to.Req = hw.Req
	to.Ack = hw.Ack
	if hw.Data != "" {
		to.Data = hw.Data
	}
	return to, nil
}

func (c *HandshakeChannel) getStv() string {
	ret := "std_logic_vector(DATA_WIDTH -1 downto 0)"
	if c.DataWidth != 0 {
		ret = "std_logic_vector(" + strconv.Itoa(c.DataWidth) + "-1 downto 0)"
	}
	return ret
}

func SignalsString(c *HandshakeChannel) string {
	ret := "signal " + c.Req + ", " + c.Ack + " : std_logic;"
	if c.Data != "" {
		ret += "signal " + c.Data + ": " + c.getStv() + ";"
	}

	return ret + "\n"
}
