package components

import (
	"fmt"
	"go2async/pkg/variable"
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

	variables map[string]*variable.VariableInfo

	bodyReg *Reg
	body    BodyComponent

	predecessor   BodyComponent
	variablesSize int
}

var loopBlockNr = 0

func NewLoopBlock(loopCond *SelectorBlock, body, predecessor BodyComponent) *LoopBlock {
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
	lb.condFork = NewFork("1")
	lb.loopCond.Out.Connect(lb.condFork.In)
	lb.condFork.Out1.Data = "open"
	lb.condFork.Out2.Data = "open"

	//to mux
	lb.condReg = NewReg(&one, true, "1")
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

	lb.variablesSize = *predecessor.GetVariablesSize()

	lb.Out.DataWidth = &lb.variablesSize

	lb.bodyReg = NewReg(&lb.variablesSize, false, "0")

	body.OutChannel().Connect(lb.entryMux.In2)

	lb.exitDemux.Out2.Connect(lb.bodyReg.In)
	lb.bodyReg.Out.Connect(body.InChannel())

	lb.variables = make(map[string]*variable.VariableInfo)

	lb.predecessor = predecessor

	fmt.Printf("loop  block %d created with width %d\n", loopBlockNr-1, lb.variablesSize)

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
    DATA_WIDTH => ` + strconv.Itoa(*lb.GetVariablesSize()) + `
  )
  port map (
    rst => rst,
    in_ack => ` + lb.In.Ack + `,
    in_req => ` + lb.In.Req + `,
    in_data => std_logic_vector(resize(unsigned(` + lb.In.Data + `), ` + strconv.Itoa(*lb.GetVariablesSize()) + `)),
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

func (lb *LoopBlock) ScopedVariables() map[string]*variable.VariableInfo {
	return lb.variables
}

func (lb *LoopBlock) Predecessor() BodyComponent {
	return lb.predecessor
}

func (lb *LoopBlock) GetVariablesSize() *int {
	return &lb.variablesSize
}
