package components

import (
	"go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"strconv"
	"strings"
)

const loopBlockPrefix = "LB_"

type LoopBlock struct {
	Block

	entryMux *MUX

	initRegFork *RegFork
	loopCond    *SelectorBlock
	condFork    *Fork
	condReg     *Reg

	exitDemux *Demux

	bodyReg *Reg
	body    BodyComponentType
}

var loopBlockNr = 0

func NewLoopBlock(parent BlockType) *LoopBlock {
	lb := &LoopBlock{
		Block: Block{
			BodyComponent: BodyComponent{

				parentBlock: parent,

				inputVariables: NewScopedVariables(),
				// outputVariables: NewScopedVariables(),

				predecessors: map[string]BodyComponentType{},
				successors:   map[string]BodyComponentType{},

				isBlock: true,
			},

			ExternalInterfaces: make(map[string]*variable.VariableInfo),
			VariableOwner:      make(map[string]*variableOwner),

			TopLevel:              false,
			KeepVariableOwnership: false,
		},
	}

	lb.number = loopBlockNr
	loopBlockNr++
	lb.name = strings.ToLower(loopBlockPrefix + strconv.Itoa(lb.number))
	lb.archName = archPrefix + lb.name

	inputChannel := NewDefaultInputHandshakeChannel(lb)
	lb.In = append(lb.In, inputChannel)

	outputChannel := NewDefaultOutputHandshakeChannel(lb)
	lb.Out = append(lb.Out, outputChannel)

	lb.InData = append(lb.InData, NewDefaultInDataChannel(lb, lb.InputVariables()))
	lb.OutData = append(lb.OutData, NewDefaultOutDataChannel(lb, lb.OutputVariables()))

	// Inner channels -> directions from in/out are handled reversed!
	innerInputChannel := NewOutputHandshakeChannel(lb, "in_req", "in_ack")
	lb.InnerInChannel = append(lb.InnerInChannel, innerInputChannel)

	innerOutputChannel := NewInputHandshakeChannel(lb, "out_req", "out_ack")
	lb.InnerOutChannel = append(lb.InnerOutChannel, innerOutputChannel)

	innerInDataChannel := NewOutDataChannel(lb, lb.InputVariables(), "in_data")
	innerInDataChannel.Connected = true
	lb.InnerInDataChannel = append(lb.InnerInDataChannel, innerInDataChannel)

	innerOutDataChannel := NewInDataChannel(lb, lb.OutputVariables(), "out_data")
	innerOutDataChannel.Connected = true
	lb.InnerOutDataChannel = append(lb.InnerOutDataChannel, innerOutDataChannel)

	infoPrinter.DebugPrintfln("[%s]: Added loopblock", lb.Name())

	return lb
}

func (lb *LoopBlock) Name() string {
	return lb.name
}

func (lb *LoopBlock) ComponentStr() string {
	return lb.Name() + `: entity work.LoopBlock(` + lb.archName + `)
  generic map(
    DATA_WIDTH => ` + strconv.Itoa(lb.GetVariableSize()) + `
  )
  port map (
	  -- Input Channel
	  in_req => ` + lb.In[0].GetReqSignalName() + `,
	  in_ack => ` + lb.In[0].GetAckSignalName() + `,
	  in_data => ` + lb.InData[0].GetDataSignalName() + `,

	  -- Output Channel
	  out_req => ` + lb.Out[0].GetReqSignalName() + `,
	  out_ack => ` + lb.Out[0].GetAckSignalName() + `,
	  out_data => ` + lb.OutData[0].GetDataSignalName() + `,

	  rst => rst
  );
  `
}

/* func (lb *LoopBlock) signalDefs() string {
	ret := lb.body.OutChannel().SignalsString()
	ret += lb.entryMux.Out.SignalsString()
	ret += lb.initRegFork.Out1.SignalsString()
	ret += lb.initRegFork.Out2.SignalsString()
	ret += lb.loopCond.Out.SignalsString()
	ret += lb.condFork.Out1.SignalsString()
	ret += lb.condFork.Out2.SignalsString()
	ret += lb.exitDemux.Out1.SignalsString()
	ret += lb.exitDemux.Out2.SignalsString()
	ret += lb.bodyReg.Out.SignalsString()

	ret += "signal " + lb.condReg.Out.Req + ", " + lb.condReg.Out.Ack + " : std_logic;"
	ret += "signal " + lb.condReg.Out.Data + " : std_logic_vector(0 downto 0);\n"
	return ret
} */

func (lb *LoopBlock) Architecture() string {
	lb.createInnerLife()

	if lb.entryMux == nil {
		panic("missing entryMux in ifBlock")
	}
	if lb.initRegFork == nil {
		panic("missing initRegFork in ifBlock")
	}
	/* if lb.condFork == nil {
		panic("missing condFork in ifBlock")
	} */
	if lb.condReg == nil {
		panic("missing condReg in ifBlock")
	}
	if lb.exitDemux == nil {
		panic("missing exitDemux in ifBlock")
	}
	if lb.bodyReg == nil {
		panic("missing bodyReg in ifBlock")
	}
	if lb.body == nil {
		panic("missing body in ifBlock")
	}
	if lb.loopCond == nil {
		panic("missing loopCond in ifBlock")
	}

	handshakeSignalAssignments := lb.getHandshakeSignalAssignments()

	dataSignalAssignments := lb.getDataSignalAssignments()

	signalDefs := ""
	for _, rbp := range lb.RegBlockPairs {
		comp := rbp.Bc

		signalDefs += comp.GetSignalDefs()

		signalDefs += "\n"
	}
	signalDefs += "\n"

	handshakeSignalAssignments += lb.GetInnerHandshakeSignalAssignmentstr()

	componentStr := lb.componentsString()

	signalDefs += lb.GetInnerSignalDefs()
	signalDefs += "\n"

	ret := "architecture " + lb.archName + " of " + lb.EntityName() + " is"
	ret += "\n"
	ret += signalDefs
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += "signalAssignments: process(all)"
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += handshakeSignalAssignments
	ret += "\n"

	ret += dataSignalAssignments
	ret += "\n"

	ret += "\n"

	ret += "end process;"

	ret += "\n"

	ret += componentStr

	ret += "end " + lb.archName + ";"

	return ret
}

func (lb *LoopBlock) EntityName() string {
	return "LoopBlock"
}

func (lb *LoopBlock) Entity() string {
	return `LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.click_element_library_constants.ALL;

ENTITY ` + lb.EntityName() + ` IS
  GENERIC (
    DATA_WIDTH : NATURAL := 8
  );
  PORT (
    rst : IN STD_LOGIC;
    -- Input channel
    in_req : IN STD_LOGIC;
    in_ack : OUT STD_LOGIC;
    in_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
    -- Output channel
    out_req : OUT STD_LOGIC;
    out_ack : IN STD_LOGIC;
    out_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0));
END ` + lb.EntityName() + `;`
}

func (lb *LoopBlock) AssignBodyComponents(loopCond *SelectorBlock, body BodyComponentType) {
	lb.loopCond = loopCond
	lb.body = body

	infoPrinter.DebugPrintfln("[%s]: added loopCond %s and body %s", lb.Name(), lb.loopCond.Name(), lb.body.Name())
}

func (lb *LoopBlock) createInnerLife() {
	if lb.body == nil {
		panic("missing body in ifBlock")
	}
	if lb.loopCond == nil {
		panic("missing loopCond in ifBlock")
	}

	infoPrinter.DebugPrintfln("[%s]: creating inner life", lb.Name())

	lb.entryMux = NewMUX(lb)
	lb.initRegFork = NewRegFork(lb)
	lb.exitDemux = NewDemux(lb)
	//lb.condFork = NewFork(lb)

	lb.condReg = NewReg(lb, lb.loopCond.OutputVariables(), true, "1")
	lb.bodyReg = NewReg(lb, lb.exitDemux.OutputVariables(), false, "0")

	// EntryMUX
	lb.entryMux.ConnectHandshakePos(lb, 0, 0)
	lb.entryMux.ConnectDataPos(lb, 0, 0)

	// Default in2
	lb.entryMux.ConnectDataPos(lb, 1, 0)

	lb.entryMux.ConnectDataPos(lb.body, 1, 0)

	// InitRegFork
	lb.initRegFork.ConnectHandshake(lb.entryMux)
	lb.initRegFork.ConnectData(lb.entryMux)

	// Selector path
	lb.loopCond.ConnectHandshakePos(lb.initRegFork, 0, 0)
	lb.loopCond.ConnectDataPos(lb.initRegFork, 0, 0)
	lb.loopCond.ConnectDataPos(lb.initRegFork, 1, 0)

	// lb.condFork.ConnectHandshake(lb.loopCond)
	// lb.condFork.ConnectData(lb.loopCond)

	lb.condReg.ConnectHandshakePos(lb.loopCond, 0, 0)
	lb.condReg.ConnectDataPos(lb.loopCond, 0, 0)

	lb.entryMux.ConnectHandshakePos(lb.condReg, 2, 0)
	lb.entryMux.ConnectDataPos(lb.condReg, 2, 0)

	// Exit
	lb.exitDemux.ConnectHandshakePos(lb.initRegFork, 0, 1)
	lb.exitDemux.ConnectDataPos(lb.initRegFork, 0, 1)

	lb.exitDemux.ConnectHandshakePos(lb.loopCond, 1, 0)
	lb.exitDemux.ConnectDataPos(lb.loopCond, 1, 0)

	// Body
	lb.bodyReg.ConnectHandshakePos(lb.exitDemux, 0, 1)
	lb.bodyReg.ConnectDataPos(lb.exitDemux, 0, 1)

	lb.body.ConnectHandshake(lb.bodyReg)
	lb.body.ConnectData(lb.bodyReg)

	lb.entryMux.ConnectHandshakePos(lb.body, 1, 0)

	lb.RegBlockPairs = append(lb.RegBlockPairs, &regBodyPair{Bc: lb.entryMux})
	lb.RegBlockPairs = append(lb.RegBlockPairs, &regBodyPair{Bc: lb.initRegFork})
	//lb.RegBlockPairs = append(lb.RegBlockPairs, &regBodyPair{Bc: lb.condFork})
	lb.RegBlockPairs = append(lb.RegBlockPairs, &regBodyPair{Bc: lb.loopCond})
	lb.RegBlockPairs = append(lb.RegBlockPairs, &regBodyPair{Bc: lb.condReg})
	lb.RegBlockPairs = append(lb.RegBlockPairs, &regBodyPair{Bc: lb.exitDemux})
	lb.RegBlockPairs = append(lb.RegBlockPairs, &regBodyPair{Bc: lb.bodyReg})
	lb.RegBlockPairs = append(lb.RegBlockPairs, &regBodyPair{Bc: lb.body})
}

/* entryIn := &HandshakeChannel{
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
lb.condFork = NewFork(nil)
lb.loopCond.Out.Connect(lb.condFork.In)
lb.condFork.Out1.Data = "open"
lb.condFork.Out2.Data = "open"

//to mux
lb.condReg = NewReg(one, true, "1")
lb.condFork.Out1.Connect(lb.condReg.In)
lb.condReg.In.Data = lb.loopCond.Out.Data

lb.condReg.Out.Connect(lb.entryMux.Select)
//to demux

lb.exitDemux = NewDEMUX(-1)
lb.exitDemux.Out1 = lb.OutChannel()
lb.condFork.Out2.Connect(lb.exitDemux.Select)
lb.exitDemux.Select.Data = lb.loopCond.Out.Data

lb.initRegFork.Out2.Connect(lb.exitDemux.In)

lb.body = body

//lb.Out.DataWidth = &lb.variablesSize

lb.bodyReg = NewReg(lb.GetVariableSize(), false, "0")

body.OutChannel().Connect(lb.entryMux.In2)

lb.exitDemux.Out2.Connect(lb.bodyReg.In)
lb.bodyReg.Out.Connect(body.InChannel()) */
