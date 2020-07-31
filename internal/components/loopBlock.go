package components

import (
	"strconv"
	"strings"
)

const loopBlockPrefix = "LB_"

type LoopBlock struct {
	Nr       int
	archName string

	entryMux    *MUX
	initRegFork *RegFork
	loopCond    *SelectorBlock
	condFork    *Fork
	condReg     *Reg
	exitDemux   *DEMUX

	In  *HandshakeChannel
	Out *HandshakeChannel

	bodyReg *Reg
	body    BodyComponent
}

var loopBlockNr = 0

func NewLoopBlock(loopCond *SelectorBlock, body BodyComponent) *LoopBlock {
	nr := loopBlockNr
	loopBlockNr++

	name := strings.ToLower(loopBlockPrefix + strconv.Itoa(nr))
	lb := &LoopBlock{
		Nr:       nr,
		archName: archPrefix + name,
		In: &HandshakeChannel{
			Out: false,
		},
		Out: &HandshakeChannel{
			Req:  name + "_o_req",
			Ack:  name + "_o_ack",
			Data: name + "_data",
			Out:  true,
		},
	}

	entryIn := &HandshakeChannel{
		Req:  "in_req",
		Ack:  "in_ack",
		Data: "in_data",
		Out:  true,
	}

	lb.loopCond = loopCond

	lb.entryMux = NewMUX()
	lb.entryMux.In1 = entryIn
	lb.initRegFork = NewRegFork()
	lb.entryMux.Out.Connect(lb.initRegFork.In)

	lb.initRegFork.Out1.Connect(lb.loopCond.In)
	lb.condFork = NewFork()
	lb.loopCond.Out.Connect(lb.condFork.In)

	//to mux
	lb.condReg = NewReg("1", true)
	lb.condFork.Out1.Connect(lb.condReg.In)
	lb.condReg.In.Data = lb.loopCond.Out.Data

	lb.condReg.Out.Connect(lb.entryMux.Select)
	//to demux

	lb.exitDemux = NewDEMUX()
	lb.exitDemux.Out1 = lb.OutChannel()
	lb.condFork.Out2.Connect(lb.exitDemux.Select)
	lb.exitDemux.Select.Data = lb.loopCond.Out.Data

	lb.initRegFork.Out2.Connect(lb.exitDemux.In)

	lb.body = body

	lb.bodyReg = NewReg("", false)

	body.OutChannel().Connect(lb.entryMux.In2)

	lb.exitDemux.Out2.Connect(lb.bodyReg.In)
	lb.bodyReg.Out.Connect(body.InChannel())

	return lb
}

func (lb *LoopBlock) InChannel() *HandshakeChannel {
	return lb.In
}

func (lb *LoopBlock) OutChannel() *HandshakeChannel {
	return lb.Out
}

func (lb *LoopBlock) Component() string {
	name := loopBlockPrefix + strconv.Itoa(lb.Nr)
	return name + `: entity work.LoopBlock(` + lb.archName + `)
  generic map(
	VARIABLE_WIDTH => VARIABLE_WIDTH,
    DATA_WIDTH => DATA_WIDTH,
    DATA_MULTIPLIER => DATA_MULTIPLIER
  )
  port map (
    rst => rst,
    in_ack => ` + lb.In.Ack + `,
    in_req => ` + lb.In.Req + `,
    in_data => ` + lb.In.Data + `,
    -- Output channel
    out_req => ` + lb.Out.Req + `,
    out_data => ` + lb.Out.Data + `,
    out_ack => ` + lb.Out.Ack + `
  );
  `
}

func (lb *LoopBlock) signalDefs() string {
	ret := SignalsString(lb.body.OutChannel())
	ret += SignalsString(lb.entryMux.Out)
	ret += SignalsString(lb.initRegFork.Out1)
	ret += SignalsString(lb.initRegFork.Out2)
	ret += SignalsString(lb.loopCond.Out)
	ret += SignalsString(lb.condFork.Out1)
	ret += SignalsString(lb.condFork.Out2)
	ret += SignalsString(lb.exitDemux.Out1)
	ret += SignalsString(lb.exitDemux.Out2)
	ret += SignalsString(lb.bodyReg.Out)

	ret += "signal " + lb.condReg.Out.Req + ", " + lb.condReg.Out.Ack + " : std_logic;"
	ret += "signal " + lb.condReg.Out.Data + " : std_logic_vector(0 downto 0);\n"
	return ret
}

func (lb *LoopBlock) Architecture() string {
	// TODO: add inner components
	ret := `architecture ` + lb.archName + ` of LoopBlock is
	`
	ret += lb.signalDefs()

	ret += "\n"

	ret += "begin"

	ret += "\n"

	ret += "out_req <= " + lb.OutChannel().Req + "; \n"
	ret += lb.OutChannel().Ack + " <= out_ack; \n"
	ret += "out_data <= " + lb.OutChannel().Data + "; \n"

	ret += "\n"

	ret += lb.entryMux.Component()
	ret += "\n"
	ret += lb.initRegFork.Component()
	ret += "\n"
	ret += lb.loopCond.Component()
	ret += "\n"
	ret += lb.condFork.Component()
	ret += "\n"
	ret += lb.condReg.Component()
	ret += "\n"
	ret += lb.exitDemux.Component()
	ret += "\n"
	ret += lb.bodyReg.Component()
	ret += "\n"
	ret += lb.body.Component()
	ret += "\n"

	ret += `end ` + lb.archName + `;
	`
	return ret
}

func (lb *LoopBlock) ArchName() string {
	return lb.archName
}
