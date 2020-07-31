package components

import (
	"strconv"
	"strings"
)

const blockPrefix = "B_"

type Block struct {
	Nr       int
	archName string

	components []BodyComponent

	In  *HandshakeChannel
	Out *HandshakeChannel
}

var blockNr = 0

func NewBlock() *Block {
	nr := blockNr
	blockNr++

	name := strings.ToLower(blockPrefix + strconv.Itoa(nr))
	b := &Block{
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

	return b
}

func (b *Block) InChannel() *HandshakeChannel {
	return b.In
}

func (b *Block) OutChannel() *HandshakeChannel {
	return b.Out
}

func (b *Block) AddComponent(c BodyComponent) {
	b.Out = c.OutChannel()
	if len(b.components) == 0 {
		entryIn := &HandshakeChannel{
			Req:  "in_req",
			Ack:  "in_ack",
			Data: "in_data",
			Out:  true,
		}
		*c.InChannel() = *entryIn
	} else {
		b.Out = c.OutChannel()
		b.components[len(b.components)-1].OutChannel().Connect(c.InChannel())
	}
	b.components = append(b.components, c)
}

func (b *Block) Component() string {
	name := blockPrefix + strconv.Itoa(b.Nr)
	return name + `: entity work.BlockC(` + b.archName + `)
  generic map(
   VARIABLE_WIDTH => VARIABLE_WIDTH,
   DATA_WIDTH => DATA_WIDTH,
   DATA_MULTIPLIER => DATA_MULTIPLIER
  )
  port map (
   rst => rst,
   in_ack => ` + b.In.Ack + `,
   in_req => ` + b.In.Req + `,
   in_data => ` + b.In.Data + `,
   -- Output channel
   out_req => ` + b.Out.Req + `,
   out_data => ` + b.Out.Data + `,
   out_ack => ` + b.Out.Ack + `
  );
  `
}

func (b *Block) signalDefs() string {
	if len(b.components) == 0 {
		return ""
	}

	ret := ""

	for _, c := range b.components {
		ret += SignalsString(c.OutChannel())
	}

	return ret
}

func (b *Block) ioChannels() string {
	ret := ""
	if len(b.components) == 0 {
		ret += "out_req <= in_req; \n"
		ret += "in_ack <= out_ack; \n"
		ret += "out_data <= in_data; \n"
	} else {

		ret += "out_req <= " + b.OutChannel().Req + "; \n"
		ret += b.OutChannel().Ack + " <= out_ack; \n"
		ret += "out_data <= " + b.OutChannel().Data + "; \n"
	}

	return ret
}

func (b *Block) componentsString() string {
	ret := ""
	for _, c := range b.components {
		ret += c.Component()
	}

	return ret
}

func (b *Block) Architecture() string {
	// TODO: add inner components
	ret := `architecture ` + b.archName + ` of BlockC is
	`

	ret += b.signalDefs()
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += b.ioChannels()

	ret += "\n"

	ret += b.componentsString()

	ret += `end ` + b.archName + `;
	`
	return ret
}

func (b *Block) ArchName() string {
	return b.archName
}
