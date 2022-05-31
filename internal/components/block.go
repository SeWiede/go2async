package components

import (
	"go2async/pkg/variable"
	"strconv"
	"strings"
)

const blockPrefix = "B_"

type regBodyPair struct {
	Reg *Reg
	Bc  BodyComponentType
}

type Block struct {
	BodyComponent
	Nr int

	TopLevel       bool
	RegBlockPairs  []*regBodyPair
	BodyComponents []BodyComponentType

	scopedVariables *ScopedVariables

	OutputSize int
}

var blockNr = 0

func NewBlock(toplevel bool, parent *Block) *Block {
	nr := blockNr
	blockNr++

	name := strings.ToLower(blockPrefix + strconv.Itoa(nr))
	b := &Block{
		BodyComponent: BodyComponent{
			archName: archPrefix + name,
			In: &HandshakeChannel{
				Req:  "in_req",
				Ack:  "in_ack",
				Data: "in_data",
				Out:  false,
			},
			Out: &HandshakeChannel{
				Req:  "bl_" + strconv.Itoa(nr) + "_out_req",
				Ack:  "bl_" + strconv.Itoa(nr) + "_out_ack",
				Data: "bl_" + strconv.Itoa(nr) + "_data_out",
				Out:  true,
			},
			parent:       parent,
			variableSize: parent.GetCurrentVariableSize(),
		},

		Nr:       nr,
		TopLevel: toplevel,

		scopedVariables: NewScopedVarialbes(parent),
	}

	// TODO: handle datawidths in block
	// b.Out.DataWidth = b.GetVariablesSize()

	return b
}

func NewParamDummyBlock(params map[string]*variable.VariableInfo) *Block {
	ret := &Block{}

	ret.parent = nil
	ret.scopedVariables = &ScopedVariables{
		variables: params,
	}

	ret.scopedVariables.size = 0
	for _, v := range params {
		ret.scopedVariables.size += v.Len * v.Size
	}

	return ret
}

func (b *Block) AddComponent(c BodyComponentType) {
	b.Out = c.OutChannel()

	if b.TopLevel {
		newreg := NewReg(c.GetVariableSize(), false, "0")
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

func (b *Block) ComponentStr() string {
	name := blockPrefix + strconv.Itoa(b.Nr)

	dataInWidth := strconv.Itoa(b.GetVariableSize())
	dataOutWidth := dataInWidth
	if len(b.RegBlockPairs) > 0 {
		dataOutWidth = strconv.Itoa(b.RegBlockPairs[len(b.RegBlockPairs)-1].Bc.GetVariableSize())
	}

	return name + `: entity work.BlockC(` + b.archName + `)
  generic map(
   DATA_IN_WIDTH => ` + dataInWidth + `,
   DATA_OUT_WIDTH => ` + dataOutWidth + `
  )
  port map (
   rst => rst,
   -- Input channel
   in_ack => ` + b.In.Ack + `,
   in_req => ` + b.In.Req + `,
   in_data => std_logic_vector(resize(unsigned(` + b.In.Data + `), ` + strconv.Itoa(b.GetVariableSize()) + `)),
   -- Output channel
   out_req => ` + b.Out.Req + `,
   out_data => ` + b.Out.Data + `,
   out_ack => ` + b.Out.Ack + `
  );
  `
	//out_data => ` + b.Out.Data + `(` + b.Out.Data + `'length - 1 downto ` + b.Out.Data + `'length - ` + strconv.Itoa(*b.GetVariablesSize()) + `),
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

		/*lowerBound := "0"
		if *b.GetVariablesSize() != *b.Out.DataWidth {
			lowerBound = strconv.Itoa(*b.GetVariablesSize())
		}*/

		//ret += "out_data <= " + b.OutChannel().Data + "(" + strconv.Itoa(*b.OutChannel().DataWidth) + " - 1 downto " + lowerBound + "); \n"
		ret += "out_data <= " + b.OutChannel().Data + "(" + b.OutChannel().Data + "'length - 1 downto " + b.OutChannel().Data + "'length - out_data'length); \n"
	}

	return ret
}

func (b *Block) componentsString() string {
	ret := ""
	for _, c := range b.RegBlockPairs {
		if c.Reg != nil {
			ret += c.Reg.ComponentStr()
		}
		ret += c.Bc.ComponentStr()
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

func (b *Block) ScopedVariables() *ScopedVariables {
	return b.scopedVariables
}

func (b *Block) GetCurrentVariableSize() int {
	return b.scopedVariables.size
}

func (b *Block) NewVariable(name string, typ string, len int) (*variable.VariableInfo, error) {
	return b.ScopedVariables().AddVariable(name, typ, len)
}

func (b *Block) GetVariable(name string) (*variable.VariableInfo, error) {
	v, err := b.ScopedVariables().GetVariableInfo(name)
	if err != nil {
		if b.Parent() == nil {
			return nil, err
		}

		return b.Parent().GetVariable(name)
	} else {
		return v, nil
	}
}
