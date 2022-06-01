package components

import (
	"strconv"
	"strings"
)

const loopBlockPrefix = "LB_"

type LoopBlock struct {
	BodyComponent

	Nr int

	entryMux *MUX

	initRegFork *RegFork
	loopCond    *SelectorBlock
	condFork    *Fork
	condReg     *Reg
	exitDemux   *DEMUX

	bodyReg *Reg
	body    BodyComponentType
}

var loopBlockNr = 0

func NewLoopBlock(loopCond *SelectorBlock, body BodyComponentType, parent *Block) *LoopBlock {
	nr := loopBlockNr
	loopBlockNr++

	name := strings.ToLower(loopBlockPrefix + strconv.Itoa(nr))
	lb := &LoopBlock{
		BodyComponent: BodyComponent{
			archName: archPrefix + name,
			In: &HandshakeChannel{
				Out: false,
			},
			Out: &HandshakeChannel{
				Req:       name + "_o_req",
				Ack:       name + "_o_ack",
				Data:      name + "_data",
				Out:       true,
				DataWidth: parent.GetCurrentVariableSize(),
			},

			parent:       parent,
			variableSize: parent.GetCurrentVariableSize(),
		},
		Nr: nr,
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
	lb.condReg = NewReg(one, true, "1")
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

	//lb.Out.DataWidth = &lb.variablesSize

	lb.bodyReg = NewReg(lb.GetVariableSize(), false, "0")

	body.OutChannel().Connect(lb.entryMux.In2)

	lb.exitDemux.Out2.Connect(lb.bodyReg.In)
	lb.bodyReg.Out.Connect(body.InChannel())

	return lb
}

func (lb *LoopBlock) ComponentStr() string {
	name := loopBlockPrefix + strconv.Itoa(lb.Nr)

	return name + `: entity work.LoopBlock(` + lb.archName + `)
  generic map(
    DATA_WIDTH => ` + strconv.Itoa(lb.GetVariableSize()) + `
  )
  port map (
    rst => rst,
    in_ack => ` + lb.In.Ack + `,
    in_req => ` + lb.In.Req + `,
    in_data => std_logic_vector(resize(unsigned(` + lb.In.Data + `), ` + strconv.Itoa(lb.GetVariableSize()) + `)),
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

	ret += lb.entryMux.ComponentStr()
	ret += "\n"
	ret += lb.initRegFork.ComponentStr()
	ret += "\n"
	ret += lb.loopCond.ComponentStr()
	ret += "\n"
	ret += lb.condFork.ComponentStr()
	ret += "\n"
	ret += lb.condReg.ComponentStr()
	ret += "\n"
	ret += lb.exitDemux.ComponentStr()
	ret += "\n"
	ret += lb.bodyReg.ComponentStr()
	ret += "\n"
	ret += lb.body.ComponentStr()
	ret += "\n"

	ret += `end ` + lb.archName + `;
	`
	return ret
}
