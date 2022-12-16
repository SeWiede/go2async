package components

import (
	"errors"
	"strconv"
)

type HandshakeChannel struct {
	Req      string
	ReqWidth int

	Ack      string
	AckWidth int

	Data          string
	DataWidth     int
	DataWidthName string

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
	if c.DataWidthName == "" {
		c.DataWidthName = "DATA_WIDTH"
	}

	ret := "std_logic_vector(" + c.DataWidthName + " - 1 downto 0)"
	if c.DataWidth > 0 {
		ret = "std_logic_vector(" + strconv.Itoa(c.DataWidth) + "-1 downto 0)"
	}
	return ret
}

func (c *HandshakeChannel) SignalsString() string {
	ret := ""

	if c.ReqWidth <= 0 {
		ret += "signal " + c.Req + " : std_logic;"
	} else {
		ret += "signal " + c.Req + " : std_logic_vector(" + strconv.Itoa(c.ReqWidth) + " - 1 downto 0);"
	}
	if c.AckWidth <= 0 {
		ret += "signal " + c.Ack + " : std_logic;"
	} else {
		ret += "signal " + c.Ack + " : std_logic_vector(" + strconv.Itoa(c.AckWidth) + " - 1 downto 0);"
	}

	if c.Data != "" {
		if c.Data != "open" {
			ret += "signal " + c.Data + ": " + c.getStv() + ";"
		}
	}

	return ret + "\n"
}
