package components

import (
	"strconv"
	"strings"
)

const ifBlockPrefix = "IF_"

type IfBlock struct {
	Nr       int
	archName string

	entryRegFork *RegFork
	cond         *SelectorBlock
	demux        *DEMUX
	thenBody     BodyComponent
	elseBody     BodyComponent
	merger       *Merge

	In  *HandshakeChannel
	Out *HandshakeChannel
}

var ifBlockNr = 0

func NewIfBlock(cond *SelectorBlock, thenBody, elseBody BodyComponent) *IfBlock {
	nr := ifBlockNr
	ifBlockNr++

	name := strings.ToLower(ifBlockPrefix + strconv.Itoa(nr))
	ib := &IfBlock{
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

	ib.thenBody = thenBody
	ib.elseBody = elseBody

	entryIn := &HandshakeChannel{
		Req:  "in_req",
		Ack:  "in_ack",
		Data: "in_data",
		Out:  true,
	}

	ib.entryRegFork = NewRegFork()
	ib.entryRegFork.In = entryIn

	ib.cond = cond
	ib.entryRegFork.Out1.Connect(ib.cond.In)

	ib.demux = NewDEMUX()
	ib.entryRegFork.Out2.Connect(ib.demux.In)
	ib.cond.Out.Connect(ib.demux.Select)

	ib.demux.Out1.Connect(thenBody.InChannel())
	ib.demux.Out2.Connect(elseBody.InChannel())

	ib.merger = NewMerge()
	ib.thenBody.OutChannel().Connect(ib.merger.In1)
	ib.elseBody.OutChannel().Connect(ib.merger.In2)

	ib.merger.Out = ib.Out

	return ib
}

func (ib *IfBlock) InChannel() *HandshakeChannel {
	return ib.In
}

func (ib *IfBlock) OutChannel() *HandshakeChannel {
	return ib.Out
}

func (ib *IfBlock) Component() string {
	name := ifBlockPrefix + strconv.Itoa(ib.Nr)
	return name + `: entity work.IfBlock(` + ib.archName + `)
  generic map(
    DATA_WIDTH => DATA_WIDTH,
    DATA_MULTIPLIER => DATA_MULTIPLIER
  )
  port map (
    rst => rst,
    in_ack => ` + ib.In.Ack + `,
    in_req => ` + ib.In.Req + `,
    in_data => ` + ib.In.Data + `,
    -- Output channel
    out_req => ` + ib.Out.Req + `,
    out_data => ` + ib.Out.Data + `,
    out_ack => ` + ib.Out.Ack + `
  );
  `
}

func (ib *IfBlock) signalDefs() string {
	ret := SignalsString(ib.entryRegFork.Out1)
	ret += SignalsString(ib.entryRegFork.Out2)
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

	ret += ib.entryRegFork.Component()
	ret += "\n"
	ret += ib.cond.Component()
	ret += "\n"
	ret += ib.demux.Component()
	ret += "\n"
	ret += ib.thenBody.Component()
	ret += "\n"
	ret += ib.elseBody.Component()
	ret += "\n"
	ret += ib.merger.Component()
	ret += "\n"

	ret += `end ` + ib.archName + `;
	`
	return ret
}

func (ib *IfBlock) ArchName() string {
	return ib.archName
}
