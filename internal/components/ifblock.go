package components

import (
	"strconv"
	"strings"
)

const ifBlockPrefix = "IF_"

type IfBlock struct {
	BodyComponent

	Nr int

	entryFork *Fork
	cond      *SelectorBlock
	demux     *DEMUX
	thenBody  BodyComponentType
	elseBody  BodyComponentType
	merger    *Merge
}

var ifBlockNr = 0

func NewIfBlock(cond *SelectorBlock, thenBody, elseBody BodyComponentType, parent *Block) *IfBlock {
	nr := ifBlockNr
	ifBlockNr++

	name := strings.ToLower(ifBlockPrefix + strconv.Itoa(nr))
	ib := &IfBlock{
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
			variableSize: parent.GetCurrentVariableSize(),
			parent:       parent,
		},
		Nr: nr,
	}

	ib.thenBody = thenBody
	ib.elseBody = elseBody

	entryIn := &HandshakeChannel{
		Req:  "in_req",
		Ack:  "in_ack",
		Data: "in_data",
		Out:  true,
	}

	ib.entryFork = NewFork("DATA_WIDTH")
	ib.entryFork.In = entryIn

	ib.cond = cond
	ib.entryFork.Out1.Connect(ib.cond.In)

	ib.demux = NewDEMUX()
	ib.entryFork.Out2.Connect(ib.demux.In)
	ib.cond.Out.Connect(ib.demux.Select)

	ib.demux.Out1.Connect(thenBody.InChannel())
	ib.demux.Out2.Connect(elseBody.InChannel())

	ib.merger = NewMerge()
	ib.thenBody.OutChannel().Connect(ib.merger.In1)
	ib.elseBody.OutChannel().Connect(ib.merger.In2)

	ib.merger.Out = ib.Out

	return ib
}

func (ib *IfBlock) ComponentStr() string {
	name := ifBlockPrefix + strconv.Itoa(ib.Nr)

	return name + `: entity work.IfBlock(` + ib.archName + `)
  generic map(
    DATA_WIDTH => ` + strconv.Itoa(ib.GetVariableSize()) + `
  )
  port map (
    rst => rst,
    in_ack => ` + ib.In.Ack + `,
    in_req => ` + ib.In.Req + `,
    in_data => std_logic_vector(resize(unsigned(` + ib.In.Data + `), ` + strconv.Itoa(ib.GetVariableSize()) + `)),
    -- Output channel
    out_req => ` + ib.Out.Req + `,
    out_data => ` + ib.Out.Data + `,
    out_ack => ` + ib.Out.Ack + `
  );
  `
}

func (ib *IfBlock) signalDefs() string {
	ret := SignalsString(ib.entryFork.Out1)
	ret += SignalsString(ib.entryFork.Out2)
	ret += SignalsString(ib.demux.Out1)
	ret += SignalsString(ib.demux.Out2)
	ret += SignalsString(ib.thenBody.OutChannel())
	ret += SignalsString(ib.elseBody.OutChannel())
	ret += SignalsString(ib.merger.Out)

	ret += "signal " + ib.cond.Out.Req + ", " + ib.cond.Out.Ack + " : std_logic;"
	ret += "signal " + ib.cond.Out.Data + " : std_logic_vector(0 downto 0);\n"
	return ret
}

func (ib *IfBlock) Architecture() string {
	// TODO: add inner components
	ret := `architecture ` + ib.archName + ` of IfBlock is
	`

	ret += ib.signalDefs()
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += "out_req <= " + ib.OutChannel().Req + "; \n"
	ret += ib.OutChannel().Ack + " <= out_ack; \n"
	ret += "out_data <= " + ib.OutChannel().Data + "; \n"

	ret += "\n"

	ret += ib.entryFork.ComponentStr()
	ret += "\n"
	ret += ib.cond.ComponentStr()
	ret += "\n"
	ret += ib.demux.ComponentStr()
	ret += "\n"
	ret += ib.thenBody.ComponentStr()
	ret += "\n"
	ret += ib.elseBody.ComponentStr()
	ret += "\n"
	ret += ib.merger.ComponentStr()
	ret += "\n"

	ret += `end ` + ib.archName + `;
	`
	return ret
}
