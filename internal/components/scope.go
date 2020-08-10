package components

import (
	"go2async/pkg/variable"
	"strconv"
	"strings"
)

const scopePrefix = "SC_"

type Scope struct {
	Nr       int
	archName string

	Block *Block

	Params     map[string]*variable.VariableInfo
	Variables  map[string]*variable.VariableInfo
	ReturnVars []*variable.VariableInfo

	OutReg *Reg

	In  *HandshakeChannel
	Out *HandshakeChannel
}

var scopeNr = 0

func NewScope(name string, block *Block, params, vars map[string]*variable.VariableInfo, returnVars []*variable.VariableInfo) *Scope {
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
		Params:     params,
		Variables:  vars,
		ReturnVars: returnVars,
	}

	entryIn := &HandshakeChannel{
		Req:  "in_req",
		Ack:  "in_ack",
		Data: "std_logic_vector(resize(unsigned(in_data), DATA_WIDTH))", // "in_data",
		Out:  true,
	}

	s.Block.In = entryIn

	rs := 0
	for _, s := range s.ReturnVars {
		rs += s.Size
	}

	s.OutReg = NewReg("DATA_WIDTH", false, "0")

	s.Block.Out.Connect(s.OutReg.In)

	s.Out = s.OutReg.Out

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
    DATA_WIDTH => ` + s.archName + `_DATA_WIDTH,
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
	ret += SignalsString(s.OutReg.Out)

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
	for _, v := range s.ReturnVars {
		if v.Len == 1 {
			idx := getIndex(v.Index)
			ret += s.OutReg.OutChannel().Data + "(" + strconv.Itoa(v.Position+v.Size*(idx+1)) + " -1 downto " + strconv.Itoa(v.Position+v.Size*idx) + ") & "
		} else {
			for idx := v.Len - 1; idx >= 0; idx-- {
				ret += s.OutReg.OutChannel().Data + "(" + strconv.Itoa(v.Position+v.Size*(idx+1)) + " -1 downto " + strconv.Itoa(v.Position+v.Size*idx) + ") & "
			}
		}
	}
	ret = strings.TrimSuffix(ret, " & ")

	ret += ";\n"

	ret += "\n"

	ret += s.Block.Component()
	ret += "\n"
	ret += s.OutReg.Component()
	ret += "\n"

	ret += `end ` + s.archName + `;
	`
	return ret
}

func (s *Scope) ArchName() string {
	return s.archName
}
