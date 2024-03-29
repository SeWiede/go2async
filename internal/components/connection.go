package components

import (
	"errors"
	"go2async/internal/globalArguments"
	"go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"strconv"
)

type DataChannel struct {
	Owner BodyComponentType

	DataName string

	variables *variable.ScopedVariables

	To map[string]*DataChannelVariable

	Connected bool
	Out       bool
}

type HandshakeChannel struct {
	Owner BodyComponentType
	Req   string
	Ack   string

	fork *MultiHsFork
	join *MultiHsJoin

	To []*HandshakeChannel

	Connected bool

	Out bool

	phaseInit bool
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

		fork: fork,
		join: join,

		Connected: false,

		Out: out,
	}
}

func NewDefaultInDataChannel(owner BodyComponentType, inputScope *variable.ScopedVariables) *DataChannel {
	return NewDataChannel(owner, inputScope, owner.Name()+"_in_data", false)
}

func NewDefaultOutDataChannel(owner BodyComponentType, inputScope *variable.ScopedVariables) *DataChannel {
	return NewDataChannel(owner, inputScope, owner.Name()+"_out_data", true)
}

func NewInDataChannel(owner BodyComponentType, inputScope *variable.ScopedVariables, dataSignalName string) *DataChannel {
	return NewDataChannel(owner, inputScope, dataSignalName, false)
}

func NewOutDataChannel(owner BodyComponentType, inputScope *variable.ScopedVariables, dataSignalName string) *DataChannel {
	return NewDataChannel(owner, inputScope, dataSignalName, true)
}

func NewDataChannel(owner BodyComponentType, inputScope *variable.ScopedVariables, dataSignalName string, out bool) *DataChannel {
	return &DataChannel{
		Owner:     owner,
		DataName:  dataSignalName,
		variables: inputScope,
		Out:       out,

		To: map[string]*DataChannelVariable{},
	}
}

func (hw *HandshakeChannel) SetPhaseInit() {
	hw.phaseInit = true
	if hw.Out {
		hw.fork.phaseInit = true
	} else {
		hw.join.phaseInit = true
	}
}

func (hw *HandshakeChannel) GetSignalAssigmentStr() string {
	if !hw.Connected {
		hw.AssignDefaultOut()
	}

	signalAssignments := ""

	if *globalArguments.Sequential {

		// default
		if hw.Out {
			signalAssignments += hw.To[0].Req + " <= " + hw.Req + ";"
			signalAssignments += "\n"
			signalAssignments += hw.Ack + " <= " + hw.To[0].Ack + ";"
			signalAssignments += "\n"
		} else {
			signalAssignments += hw.Req + " <= " + hw.To[0].Req + ";"
			signalAssignments += "\n"
			signalAssignments += hw.To[0].Ack + " <= " + hw.Ack + ";"
			signalAssignments += "\n"
		}

	} else {
		if hw.Out {
			hwFork := hw.fork

			signalAssignments += hwFork.Name() + "_in_req <= " + hw.Req + ";"
			signalAssignments += "\n"
			signalAssignments += hw.Ack + " <= " + hwFork.Name() + "_in_ack;"
			signalAssignments += "\n"

			infoPrinter.DebugPrintfln("[%s]: connecting outputs to %d forks", hw.Owner.Name(), len(hwFork.ReceiverList))

			for i, j := range hwFork.ReceiverList {
				infoPrinter.DebugPrintfln("[%s]: connecting output to %s", hw.Owner.Name(), j.Receiver.Name())
				istr := strconv.Itoa(i)
				posInJoin := hwFork.getJoinHsPos(j)
				posInJoinStr := strconv.Itoa(posInJoin)

				signalAssignments += j.Name() + "_in_req(" + posInJoinStr + ") <= " + hwFork.Name() + "_out_req(" + istr + ");"
				signalAssignments += "\n"
				signalAssignments += hwFork.Name() + "_out_ack(" + istr + ") <= " + j.Name() + "_in_ack(" + posInJoinStr + ");"
				signalAssignments += "\n"
			}
		} else {
			hwJoin := hw.join

			signalAssignments += hw.Req + " <= " + hwJoin.Name() + "_out_req;"
			signalAssignments += "\n"
			signalAssignments += hwJoin.Name() + "_out_ack <= " + hw.Ack + ";"
			signalAssignments += "\n"

			for i, f := range hwJoin.SenderList {
				istr := strconv.Itoa(i)
				posInFork := hwJoin.getForkHsPos(f)
				posInForkStr := strconv.Itoa(posInFork)

				signalAssignments += hwJoin.Name() + "_in_req(" + istr + ") <= " + f.Name() + "_out_req(" + posInForkStr + ");"
				signalAssignments += "\n"
				signalAssignments += f.Name() + "_out_ack(" + posInForkStr + ") <= " + hwJoin.Name() + "_in_ack(" + istr + ");"
				signalAssignments += "\n"
			}
		}
	}

	return signalAssignments
}

func (hw *HandshakeChannel) GetSignalAssigmentNoCheckStr() string {
	signalAssignments := ""

	// default
	/* ret += hw.Req + " <= " + hw.To[0].Req + ";\n"
	ret += hw.To[0].Ack + " <= " + hw.Ack + ";\n" */

	if hw.Out {
		if false && len(hw.To) == 1 && len(hw.To[0].To) == 1 {
			signalAssignments += hw.To[0].Req + " <= " + hw.Req + ";\n"
			signalAssignments += hw.Ack + " <= " + hw.To[0].Ack + ";\n"
		} else {
			hwFork := hw.fork

			signalAssignments += hwFork.Name() + "_in_req <= " + hw.Req + ";"
			signalAssignments += "\n"
			signalAssignments += hw.Ack + " <= " + hwFork.Name() + "_in_ack;"
			signalAssignments += "\n"

			for i, j := range hwFork.ReceiverList {
				istr := strconv.Itoa(i)
				posInJoin := hwFork.getJoinHsPos(j)
				posInJoinStr := strconv.Itoa(posInJoin)

				signalAssignments += j.Name() + "_in_req(" + posInJoinStr + ") <= " + hwFork.Name() + "_out_req(" + istr + ");"
				signalAssignments += "\n"
				signalAssignments += hwFork.Name() + "_out_ack(" + istr + ") <= " + j.Name() + "_in_ack(" + posInJoinStr + ");"
				signalAssignments += "\n"
			}
		}
	} else {
		if false && len(hw.To) == 1 && len(hw.To[0].To) == 1 {
			signalAssignments += hw.Req + " <= " + hw.To[0].Req + ";\n"
			signalAssignments += hw.To[0].Ack + " <= " + hw.Ack + ";\n"
		} else {
			hwJoin := hw.join

			signalAssignments += hw.Req + " <= " + hwJoin.Name() + "_out_req;"
			signalAssignments += "\n"
			signalAssignments += hwJoin.Name() + "_out_ack <= " + hw.Ack + ";"
			signalAssignments += "\n"

			for i, f := range hwJoin.SenderList {
				istr := strconv.Itoa(i)
				posInFork := hwJoin.getForkHsPos(f)
				posInForkStr := strconv.Itoa(posInFork)

				signalAssignments += hwJoin.Name() + "_in_req(" + istr + ") <= " + f.Name() + "_out_req(" + posInForkStr + ");"
				signalAssignments += "\n"
				signalAssignments += f.Name() + "_out_ack(" + posInForkStr + ") <= " + hwJoin.Name() + "_in_ack(" + istr + ");"
				signalAssignments += "\n"
			}
		}
	}

	return signalAssignments
}

func (hw *HandshakeChannel) ConnectHandshake(to *HandshakeChannel) {
	if to == nil {
		panic("To channel was nil")
	}
	if hw.Out == to.Out {
		infoPrinter.DebugPrintfln("[HandshakeChannel]: sdfxgb %s [%s : %s] to %s [%s : %s]", hw.Req, hw.Owner.Name(), getOutDirStr(hw.Out), to.Req, to.Owner.Name(), getOutDirStr(to.Out))

		panic("Cannot connect two channels with the same direction.")
	}

	// partner has to have the same phase!
	if hw.phaseInit {
		to.SetPhaseInit()
	} else if to.phaseInit {
		hw.SetPhaseInit()
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

	hw.Connected = true
	to.Connected = true

	infoPrinter.DebugPrintfln("[HandshakeChannel]: Connected %s [%s : %s] to %s [%s : %s]", hw.Req, hw.Owner.Name(), getOutDirStr(hw.Out), to.Req, to.Owner.Name(), getOutDirStr(to.Out))
	infoPrinter.DebugPrintfln("[HandshakeChannel]: Connected %s [%s : %s] to %s [%s : %s]", hw.Ack, hw.Owner.Name(), getOutDirStr(hw.Out), to.Ack, to.Owner.Name(), getOutDirStr(to.Out))
}

func (c *HandshakeChannel) AssignDefaultOut() {
	if !c.Out {
		panic(c.Owner.Parent().Name() + " - " + c.Owner.Name() + " input handshake " + c.Req + " is not connected!")
	}

	infoPrinter.DebugPrintfln("[%s]: assigning default OUT connection: Connect with parent - current Fork is %s", c.Owner.Name(), c.fork.Name())

	c.Owner.ConnectHandshakeDir(c.Owner.Parent(), true)
}

func (c *HandshakeChannel) SignalDefs() string {
	if !c.Connected {
		c.AssignDefaultOut()
	}

	ret := ""

	ret += "signal " + c.Req + " : std_logic;"
	ret += "signal " + c.Ack + " : std_logic;"
	ret += "\n"

	if true || len(c.To) > 1 || len(c.To[0].To) > 1 {
		ret += c.SignalDefsForkJoin()
	}

	return ret + "\n"
}

func (c *HandshakeChannel) SignalDefsForkJoin() string {
	ret := ""

	ret += "\n"
	if c.Out {
		ret += c.fork.GetSignalDefs()
	} else {
		ret += c.join.GetSignalDefs()
	}
	ret += "\n"

	return ret
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

func getOutDirStr(out bool) string {
	if out {
		return "out"
	} else {
		return "in"
	}
}

func (c *DataChannel) ConnectVariable(to *DataChannel, fromVar *variable.VariableInfo) error {
	if to == nil {
		panic("To channel was nil")
	}
	if c.Out == to.Out {
		panic("Cannot connect two channels with the same direction. Signalnames: " + c.DataName + " <=> " + to.DataName)
	}

	if !c.variables.HasVariable(fromVar) {
		infoPrinter.DebugPrintf("vars in datachannel %s: ", c.DataName)
		for _, vi := range c.variables.VariableList {
			infoPrinter.DebugPrintfln("var '%s'", vi.Name())
		}

		return errors.New("variable '" + fromVar.Name() + "' not in datachannel")
	}
	if !to.variables.HasVariable(fromVar) {
		infoPrinter.DebugPrintf("vars in datachannel %s: ", to.DataName)
		for _, vi := range to.variables.VariableList {
			infoPrinter.DebugPrintfln("var '%s'", vi.Name())
		}

		if fromVar.IndexIdent_ != nil || fromVar.Index_ != "" {

			c.To[fromVar.Name()] = &DataChannelVariable{
				dc: nil,
				vi: fromVar,
			}

			return nil
		}

		return errors.New("variable '" + fromVar.Name() + "' not in datachannel")
	}

	viFrom, _ := c.variables.GetActualVariableInfo(fromVar.Name())
	viTo, _ := to.variables.GetActualVariableInfo(fromVar.Name())

	// bidirectional
	c.To[fromVar.Name()] = &DataChannelVariable{
		dc: to,
		vi: viTo,
	}
	to.To[fromVar.Name()] = &DataChannelVariable{
		dc: c,
		vi: viFrom,
	}

	viFrom.Connected_ = true
	viTo.Connected_ = true

	infoPrinter.DebugPrintfln("[DataChannel]: Connected %s [%s : %s] to %s [%s : %s]", c.DataName, c.Owner.Name(), getOutDirStr(c.Out), to.DataName, to.Owner.Name(), getOutDirStr(to.Out))

	return nil
}

func (c *DataChannel) ConnectData(to *DataChannel) error {
	if to == nil {
		panic("To channel was nil")
	}
	if c.Out == to.Out {
		panic("Cannot connect two channels with the same direction. Signalnames: " + c.DataName + " <=> " + to.DataName)
	}

	foundAtLeastOne := false
	for _, vi := range to.variables.VariableList {
		// is allowed to fail
		if err := c.ConnectVariable(to, vi); err != nil {
			infoPrinter.DebugPrintfln("[DataChannel]: Could not connect var %s from %s to %s", vi.Name(), c.DataName, to.DataName)
		} else {
			foundAtLeastOne = true
		}
	}

	if !foundAtLeastOne {
		return errors.New("did not find at least one matching data I/O")
	}

	return nil
}

type DataChannelVariable struct {
	dc *DataChannel
	vi *variable.VariableInfo
}

func getDataChannelsThatHaveVar(searchSpace []*DataChannel, varName string) []*DataChannelVariable {
	ret := []*DataChannelVariable{}

	for _, dc := range searchSpace {
		v, err := dc.variables.GetVariableInfo(varName)

		if err == nil {
			ret = append(ret, &DataChannelVariable{
				dc: dc,
				vi: v,
			})
		}
	}

	return ret
}

func (c *DataChannel) GetSignalAssigmentStr() string {
	infoPrinter.DebugPrintfln("GETTING ASSIGNMENTS FOR %s", getOutDirStr(c.Out))

	dataSignalAssigmnent := ""

	for i, vi := range c.variables.VariableList {
		vb := vi.GetVariableVectorBounds()
		from := ""
		to := ""

		from = c.DataName + " (" + vb.UpperboundStr + " - 1 downto " + vb.LowerboundStr + ")"

		if vi.Const_ != "" {
			if c.Out {
				panic("cannot have const out")
			}

			to = "std_logic_vector(to_signed(" + vi.Const() + ", " + strconv.Itoa(vi.TotalSize()) + "))"

			dataSignalAssigmnent += from + " <= " + to + ";\n"
		} else {

			if !vi.Connected_ {
				infoPrinter.DebugPrintfln("[%s]: var '%s' not connected in %d connected dataChannels: ", c.Owner.Name(), vi.Name(), len(c.To))

				if c.Out {
					// Unconnected out connections are allowed
					continue
				}

				to = "std_logic_vector(to_signed(0, " + strconv.Itoa(vi.TotalSize()) + "))"

				dataSignalAssigmnent += from + " <= " + to + ";\n"

				infoPrinter.DebugPrintfln("[%s]: input var %s is assigned default", c.Owner.Name(), vi.Name())
			} else {

				dcv, ok := c.To[vi.Name()]
				if !ok {
					if c.Out {
						// Unconnected out connections are allowed
						continue
					}
					panic("did not find '" + vi.Name() + "' in connected dataChannels")
				}

				dcvvi := dcv.vi
				dc := dcv.dc

				dcvvb := dcvvi.GetVariableVectorBounds()

				to = dcv.dc.DataName + " (" + dcvvb.UpperboundStr + " - 1 downto " + dcvvb.LowerboundStr + ")"

				if !c.Out {
					dataSignalAssigmnent += from + " <= " + to + ";\n"
				} else {
					dataSignalAssigmnent += to + " <= " + from + ";\n"
				}

				infoPrinter.DebugPrintfln("[%s]: %d. input (out?%t)var '%s' from '%s'", c.Owner.Name(), i+1, c.Out, dcvvi.Name(), dc.Owner.Name())
			}

		}
	}

	return dataSignalAssigmnent
}
