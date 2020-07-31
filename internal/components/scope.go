package components

import (
	"strconv"
	"strings"
)

const scopePrefix = "SC_"

type Scope struct {
	Nr       int
	archName string

	Block *Block

	VarWidth        int
	VarCount        int
	ParamCount      int
	ReturnPositions []int

	In  *HandshakeChannel
	Out *HandshakeChannel
}

var scopeNr = 0

func NewScope(name string, block *Block, varWidth, varCount, paramCount int, returnPositions []int) *Scope {
	nr := scopeNr
	if name == "" {
		scopeNr++
		name = strings.ToLower(scopePrefix + strconv.Itoa(nr))
	}
	s := &Scope{
		Nr:       nr,
		archName: name,
		Block:    block,
		In: &HandshakeChannel{
			Out: false,
		},
		VarWidth:        varWidth,
		VarCount:        varCount,
		ParamCount:      paramCount,
		ReturnPositions: returnPositions,
	}

	entryIn := &HandshakeChannel{
		Req:  "in_req",
		Ack:  "in_ack",
		Data: "std_logic_vector(resize(unsigned(in_data), DATA_WIDTH))", // "in_data",
		Out:  true,
	}

	s.Block.In = entryIn

	s.Out = s.Block.Out

	return s
}

func (s *Scope) InChannel() *HandshakeChannel {
	return s.In
}

func (s *Scope) OutChannel() *HandshakeChannel {
	return s.Out
}

func (s *Scope) Component() string {
	name := scopePrefix + strconv.Itoa(s.Nr)

	return name + `: entity work.Scope(` + s.archName + `)
  generic map(
	VARIABLE_WIDTH => ` + s.archName + `_VARIABLE_WIDTH,
    DATA_WIDTH => ` + s.archName + `_DATA_WIDTH,
	DATA_MULTIPLIER => ` + s.archName + `_DATA_MULTIPLIER,
	OUT_DATA_WIDTH => ` + s.archName + `_OUT_DATA_WIDTH,
	IN_DATA_WIDTH => ` + s.archName + `_IN_DATA_WIDTH
  )
  port map (
   rst => rst,
   in_ack => ` + s.In.Ack + `,
   in_req => ` + s.In.Req + `,
   in_data => ` + s.In.Data + `,
   -- Output channel
   out_req => ` + s.Out.Req + `,
   out_data => ` + s.Out.Data + `,
   out_ack => ` + s.Out.Ack + `
  );
  `
}

func (s *Scope) signalDefs() string {

	ret := SignalsString(s.Block.Out)

	return ret
}

func (s *Scope) Architecture() string {
	// TODO: add inner components
	ret := `architecture ` + s.archName + ` of Scope is
	`

	ret += s.signalDefs()
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += "out_req <= " + s.OutChannel().Req + "; \n"
	ret += s.OutChannel().Ack + " <= out_ack; \n"
	ret += "out_data <= "
	for _, retpos := range s.ReturnPositions {
		posStr := strconv.Itoa(retpos)
		ret += s.OutChannel().Data + "((" + posStr + " + 1) * VARIABLE_WIDTH -1 downto " + posStr + " * VARIABLE_WIDTH) &"
	}
	ret = strings.TrimSuffix(ret, " &")

	ret += ";\n"

	ret += "\n"

	ret += s.Block.Component()
	ret += "\n"

	ret += `end ` + s.archName + `;
	`
	return ret
}

func (s *Scope) ArchName() string {
	return s.archName
}
