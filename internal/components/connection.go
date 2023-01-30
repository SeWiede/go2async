package components

import (
	"strconv"
)

type ChannelData struct {
	Data      string
	DataWidth int
}

type Channel struct {
	Datas []*ChannelData
}

type HandshakeChannel struct {
	Channel

	Req      string
	ReqWidth int

	Ack      string
	AckWidth int

	To *HandshakeChannel

	Out bool
}

func NewDefaultInputHandshakeChannel(componentName string) *HandshakeChannel {
	return NewHandshakeChannel(componentName+"_in_req", componentName+"_in_ack", false)
}
func NewDefaultOutputHandshakeChannel(componentName string) *HandshakeChannel {
	return NewHandshakeChannel(componentName+"_out_req", componentName+"_out_ack", true)
}

func NewInputHandshakeChannel(reqSignalName, ackSignalName string) *HandshakeChannel {
	return NewHandshakeChannel(reqSignalName, ackSignalName, false)
}

func NewOutputHandshakeChannel(reqSignalName, ackSignalName string) *HandshakeChannel {
	return NewHandshakeChannel(reqSignalName, ackSignalName, true)
}

func NewHandshakeChannel(reqSignalName, ackSignalName string, out bool) *HandshakeChannel {
	return &HandshakeChannel{
		Req:      reqSignalName,
		ReqWidth: 1,
		Ack:      ackSignalName,
		AckWidth: 1,
		Out:      out,
	}
}

func NewForkHandshakeChannel(reqSignalName, ackSignalName string, numReceivers int) *HandshakeChannel {
	return &HandshakeChannel{
		Req:      reqSignalName,
		ReqWidth: 1,
		Ack:      ackSignalName,
		AckWidth: numReceivers,
	}
}

func NewJoinHandshakeChannel(reqSignalName, ackSignalName string, numSenders int) *HandshakeChannel {
	return &HandshakeChannel{
		Req:      reqSignalName,
		ReqWidth: numSenders,
		Ack:      ackSignalName,
		AckWidth: 1,
	}
}

func (hw *HandshakeChannel) AddDataChannel(dataSignalName string, dataWidth int) {
	hw.Datas = append(hw.Datas, &ChannelData{
		Data:      dataSignalName,
		DataWidth: dataWidth,
	})
}

/* var DummyOutChannel = &HandshakeChannel{
	Req: "DUMMY_REQ",
	Ack: "DUMMY_ACK",
	Channel: Channel{
		Datas: "DUMMY_DATA",
	},
	Out: true,
}

var DummyInChannel = &HandshakeChannel{
	Req: "DUMMY_REQ",
	Ack: "DUMMY_ACK",
	Channel: Channel{
		Datas: "DUMMY_DATA",
	},
	Out: false,
} */

func (hw *HandshakeChannel) GetHandshakeAssignmentStr() string {
	ret := ""

	ret += hw.Req + " <= " + hw.To.Req
	ret += hw.To.Ack + " <= " + hw.Ack

	return ret + ";\n"
}

func (hw *HandshakeChannel) Connect(to *HandshakeChannel) {
	if to == nil {
		panic("To channel was nil")
	}
	if hw.Out == to.Out {
		panic("Cannot connect two channels with the same direction.")
	}
	/* if hw == DummyInChannel || hw == DummyOutChannel || to == DummyInChannel || to == DummyOutChannel {
		panic("Cannot connect dummy channels")
	} */

	/* to.Req = hw.Req
	to.Ack = hw.Ack

	if hw.Data != "" {
		to.Data = hw.Data
	} */

	hw.To = to
}

func (c *HandshakeChannel) SignalDefs() string {
	ret := ""

	if c.ReqWidth <= 1 {
		ret += "signal " + c.Req + " : std_logic;"
	} else {
		ret += "signal " + c.Req + " : std_logic_vector(" + strconv.Itoa(c.ReqWidth) + " - 1 downto 0);"
	}
	if c.AckWidth <= 1 {
		ret += "signal " + c.Ack + " : std_logic;"
	} else {
		ret += "signal " + c.Ack + " : std_logic_vector(" + strconv.Itoa(c.AckWidth) + " - 1 downto 0);"
	}

	for _, data := range c.Datas {
		ret += "signal " + data.Data + " : std_logic_vector(" + strconv.Itoa(data.DataWidth) + " - 1 downto 0);"
	}

	return ret + "\n"
}

func (c *HandshakeChannel) GetReqSignalName() string {
	return c.Req
}

func (c *HandshakeChannel) GetAckSignalName() string {
	return c.Ack
}

func (c *HandshakeChannel) GetDataSignalName() string {
	return c.GetDataSignalNameAt(0)
}

func (c *HandshakeChannel) GetDataSignalNameAt(pos int) string {
	if len(c.Datas) == 0 || len(c.Datas) <= pos {
		panic("invalid data pos")
	}

	return c.Datas[pos].Data
}
