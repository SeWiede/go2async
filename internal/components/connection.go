package components

import (
	"go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"strconv"
)

type DataChannel struct {
	Owner BodyComponentType

	DataName string

	variables *variable.ScopedVariables

	To []*DataChannel

	Out bool
}

type HandshakeChannel struct {
	Owner BodyComponentType
	Req   string
	Ack   string

	Width int

	fork *MultiHsFork
	join *MultiHsJoin

	To []*HandshakeChannel

	Out bool
}

func NewDefaultInputHandshakeChannel(owner BodyComponentType) *HandshakeChannel {
	return NewHandshakeChannel(owner, owner.Name()+"_in_req", owner.Name()+"_in_ack", false)
}
func NewDefaultOutputHandshakeChannel(owner BodyComponentType) *HandshakeChannel {
	return NewHandshakeChannel(owner, owner.Name()+"_out_req", owner.Name()+"_out_ack", true)
}

func NewInputHandshakeChannel(owner BodyComponentType, reqSignalName, ackSignalName string) *HandshakeChannel {
	return NewHandshakeChannel(owner, reqSignalName, ackSignalName, false)
}

func NewOutputHandshakeChannel(owner BodyComponentType, reqSignalName, ackSignalName string) *HandshakeChannel {
	return NewHandshakeChannel(owner, reqSignalName, ackSignalName, true)
}

func NewHandshakeChannel(owner BodyComponentType, reqSignalName, ackSignalName string, out bool) *HandshakeChannel {
	var join *MultiHsJoin
	var fork *MultiHsFork

	if out {
		fork = NewVariableMultiHsFork(owner)
	} else {

		join = NewVariableMultiHsJoin(owner)
	}

	return &HandshakeChannel{
		Owner: owner,
		Req:   reqSignalName,
		Ack:   ackSignalName,
		Out:   out,
		Width: 1,

		fork: fork,
		join: join,
	}
}

func NewDefaultInDataChannel(owner BodyComponentType, inputScope *variable.ScopedVariables) *DataChannel {
	return NewDataChannel(owner, inputScope, owner.Name()+"_in_data", false)
}

func NewDefaultOutDataChannel(owner BodyComponentType, inputScope *variable.ScopedVariables) *DataChannel {
	return NewDataChannel(owner, inputScope, owner.Name()+"_out_data", true)
}

func NewDataChannel(owner BodyComponentType, inputScope *variable.ScopedVariables, dataSignalName string, out bool) *DataChannel {
	return &DataChannel{
		Owner:     owner,
		DataName:  dataSignalName,
		variables: inputScope,
		Out:       out,
	}
}

func (hw *HandshakeChannel) GetSignalAssigmentStrForkAndJoins() (string, []*MultiHsFork, []*MultiHsJoin) {
	forks := []*MultiHsFork{}
	joins := []*MultiHsJoin{}

	ret := ""

	// default
	ret += hw.Req + " <= " + hw.To[0].Req + ";\n"
	ret += hw.To[0].Ack + " <= " + hw.Ack + ";\n"

	if hw.Out {
		hwFork := hw.fork
		forks = append(forks, hwFork)

		for i, j := range hwFork.ReceiverList {
			istr := strconv.Itoa(i)
			posInJoin := hwFork.getJoinHsPos(j)
			posInJoinStr := strconv.Itoa(posInJoin)

			ret += j.Name() + "_in_req(" + posInJoinStr + ") <= " + hwFork.Name() + "_out_req(" + istr + ");\n"
		}
	} else {
		hwJoin := hw.join
		joins = append(joins, hwJoin)

		for i, f := range hwJoin.SenderList {
			istr := strconv.Itoa(i)
			posInFork := hwJoin.getForkHsPos(f)
			posInforkStr := strconv.Itoa(posInFork)

			ret += hwJoin.Name() + "_in_req(" + istr + ") <= " + f.Name() + "_out_req(" + posInforkStr + ");\n"
		}
	}

	return ret + "\n", forks, joins
}

func (hw *HandshakeChannel) ConnectHandshake(to *HandshakeChannel) {
	if to == nil {
		panic("To channel was nil")
	}
	if hw.Out == to.Out {
		panic("Cannot connect two channels with the same direction.")
	}

	// bidirectional
	hw.To = append(hw.To, to)
	to.To = append(to.To, hw)

	if hw.Out {
		to.join.AddSender(hw.fork)
		hw.fork.AddReceiver(to.join)
	} else {
		hw.join.AddSender(to.fork)
		to.fork.AddReceiver(hw.join)
	}

	infoPrinter.DebugPrintfln("[HandshakeChannel]: Connected %s [%s : %s] to %s [%s : %s]", hw.Req, hw.Owner.Name(), getOutDir(hw.Out), to.Req, to.Owner.Name(), getOutDir(to.Out))
	infoPrinter.DebugPrintfln("[HandshakeChannel]: Connected %s [%s : %s] to %s [%s : %s]", hw.Ack, hw.Owner.Name(), getOutDir(hw.Out), to.Ack, to.Owner.Name(), getOutDir(to.Out))
}

func (c *HandshakeChannel) SignalDefs() string {
	ret := ""

	if c.Width <= 1 {
		ret += "signal " + c.Req + " : std_logic;"
	} else {
		ret += "signal " + c.Req + " : std_logic_vector(" + strconv.Itoa(c.Width) + " - 1 downto 0);"
	}
	if c.Width <= 1 {
		ret += "signal " + c.Ack + " : std_logic;"
	} else {
		ret += "signal " + c.Ack + " : std_logic_vector(" + strconv.Itoa(c.Width) + " - 1 downto 0);"
	}

	return ret + "\n"
}

func (c *DataChannel) SignalDefs() string {
	ret := ""

	ret += "signal " + c.DataName + " : std_logic_vector(" + strconv.Itoa(c.variables.GetSize()) + " - 1 downto 0);"

	return ret + "\n"
}

func (c *HandshakeChannel) GetReqSignalName() string {
	return c.Req
}

func (c *HandshakeChannel) GetAckSignalName() string {
	return c.Ack
}

func (c *DataChannel) GetDataSignalName() string {
	return c.DataName
}

func (c *DataChannel) AddVariable(vi *variable.VariableInfo) {
	_, err := c.variables.AddVariable(vi)
	if err != nil {
		panic(err.Error())
	}
}

func getOutDir(out bool) string {
	if out {
		return "out"
	} else {
		return "in"
	}
}

func (c *DataChannel) ConnectData(to *DataChannel) {
	if to == nil {
		panic("To channel was nil")
	}
	if c.Out == to.Out {
		panic("Cannot connect two channels with the same direction.")
	}

	// bidirectional
	c.To = append(c.To, to)
	to.To = append(to.To, c)

	infoPrinter.DebugPrintfln("[DataChannel]: Connected %s [%s : %s] to %s [%s : %s]", c.DataName, c.Owner.Name(), getOutDir(c.Out), to.DataName, to.Owner.Name(), getOutDir(to.Out))
}

type DataChannelVariable struct {
	dc *DataChannel
	vi *variable.VariableInfo
}

func getDataChannelsThatHaveVar(searchSpace []*DataChannel, varName string) []*DataChannelVariable {
	ret := []*DataChannelVariable{}

	for _, dc := range searchSpace {
		v, err := dc.variables.GetVariableInfo(varName)

		if err != nil {
			ret = append(ret, &DataChannelVariable{
				dc: dc,
				vi: v,
			})
		}
	}

	return ret
}

func (c *DataChannel) GetSignalAssigmentStr() string {
	if len(c.To) <= 0 {
		panic("invalid destination")
	}

	dataSignalAssigmnent := ""

	for _, vi := range c.variables.VariableList {
		vb := vi.GetVariableVectorBounds()
		from := ""
		to := ""

		from = c.DataName + " (" + vb.UpperboundStr + " - 1 downto " + vb.LowerboundStr + ")"

		dcvs := getDataChannelsThatHaveVar(c.To, vi.Name())
		if len(dcvs) == 0 {
			panic("var " + vi.Name() + " not found in connected dataChannels")
		}

		for _, dcv := range dcvs {
			dcvvb := dcv.vi.GetVariableVectorBounds()

			to = dcv.dc.DataName + " (" + dcvvb.UpperboundStr + " - 1 downto " + dcvvb.LowerboundStr + ")"

			if !c.Out {
				dataSignalAssigmnent += from + " <= " + to + ";\n"
			} else {
				dataSignalAssigmnent += to + " <= " + from + ";\n"
			}
		}
	}

	return dataSignalAssigmnent
}
