package components

import (
	"strconv"
	"strings"
)

const blockPrefix = "B_"

type regBodyPair struct {
	Reg *Reg
	Bc  BodyComponent
}

type Block struct {
	Nr       int
	archName string

	TopLevel      bool
	RegBlockPairs []*regBodyPair

	In  *HandshakeChannel
	Out *HandshakeChannel
}

var blockNr = 0

func NewBlock(toplevel bool) *Block {
	nr := blockNr
	blockNr++

	name := strings.ToLower(blockPrefix + strconv.Itoa(nr))
	b := &Block{
		Nr:       nr,
		archName: archPrefix + name,
		TopLevel: toplevel,
		In: &HandshakeChannel{
			Req:  "in_req",
			Ack:  "in_ack",
			Data: "in_data",
			Out:  false,
		},
		Out: &HandshakeChannel{
			Req:  "out_req",
			Ack:  "out_ack",
			Data: "out_data",
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

	if b.TopLevel {

		newreg := NewReg("DATA_WIDTH", false, "0")
		newreg.Out.Connect(c.InChannel())
		if len(b.RegBlockPairs) == 0 {
			*newreg.InChannel() = *b.In
		} else {
			b.Out = c.OutChannel()
			b.RegBlockPairs[len(b.RegBlockPairs)-1].Bc.OutChannel().Connect(newreg.InChannel())
		}
		b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{newreg, c})
	} else {
		b.Out = c.OutChannel()
		if len(b.RegBlockPairs) == 0 {
			entryIn := &HandshakeChannel{
				Req:  "in_req",
				Ack:  "in_ack",
				Data: "in_data",
				Out:  true,
			}
			*c.InChannel() = *entryIn
		} else {
			b.Out = c.OutChannel()
			b.RegBlockPairs[len(b.RegBlockPairs)-1].Bc.OutChannel().Connect(c.InChannel())
		}
		b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{Bc: c})
	}
}

func (b *Block) Component() string {
	name := blockPrefix + strconv.Itoa(b.Nr)
	return name + `: entity work.BlockC(` + b.archName + `)
  generic map(
   DATA_WIDTH => DATA_WIDTH
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
	if len(b.RegBlockPairs) == 0 {
		return ""
	}

	ret := ""

	for _, c := range b.RegBlockPairs {
		if c.Reg != nil {
			ret += SignalsString(c.Reg.OutChannel())
		}
		ret += SignalsString(c.Bc.OutChannel())
	}

	return ret
}

func (b *Block) ioChannels() string {
	ret := ""
	if len(b.RegBlockPairs) == 0 {
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
	for _, c := range b.RegBlockPairs {
		if c.Reg != nil {
			ret += c.Reg.Component()
		}
		ret += c.Bc.Component()
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
