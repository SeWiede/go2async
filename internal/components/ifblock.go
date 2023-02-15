package components

import (
	"go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"strconv"
	"strings"
)

const ifBlockPrefix = "IF_"

type IfBlock struct {
	Block

	entryFork *Fork
	cond      *SelectorBlock
	demux     *Demux
	thenBody  BodyComponentType
	elseBody  BodyComponentType
	merger    *Merge
}

var ifBlockNr = 0

func NewIfBlock(parent BlockType) *IfBlock {

	ib := &IfBlock{
		Block: Block{
			BodyComponent: BodyComponent{

				parentBlock: parent,

				inputVariables: NewScopedVariables(),
				//outputVariables: NewScopedVariables(),

				predecessors: map[string]BodyComponentType{},
				successors:   map[string]BodyComponentType{},

				isBlock: true,
			},

			ExternalInterfaces: make(map[string]*variable.VariableInfo),
			VariableOwner:      make(map[string]*variableOwner),

			TopLevel: false,
		},
	}

	ib.number = ifBlockNr
	ifBlockNr++

	ib.name = strings.ToLower(ifBlockPrefix + strconv.Itoa(ib.number))
	ib.archName = archPrefix + ib.name

	//ib.thenBody = thenBody
	//ib.elseBody = elseBody

	/*entryIn := &HandshakeChannel{
		Req:  "in_req",
		Ack:  "in_ack",
		Data: "in_data",
		Out:  true,
	}

	 ib.entryFork = NewFork("DATA_WIDTH")
	ib.entryFork.In = entryIn

	// ib.cond = cond
	// ib.entryFork.Out1.Connect(ib.cond.In)

	ib.demux = NewDEMUX()
	// ib.entryFork.Out2.Connect(ib.demux.In)
	// ib.cond.Out.Connect(ib.demux.Select)

	// ib.demux.Out1.Connect(thenBody.InChannel())
	// ib.demux.Out2.Connect(elseBody.InChannel())

	ib.merger = NewMerge()
	// ib.thenBody.OutChannel().Connect(ib.merger.In1)
	// ib.elseBody.OutChannel().Connect(ib.merger.In2)

	// ib.merger.Out = ib.Out */

	ib.entryFork = NewFork(ib)
	ib.demux = NewDemux(ib)
	ib.merger = NewMerge(ib)

	inputChannel := NewDefaultInputHandshakeChannel(ib)
	ib.In = append(ib.In, inputChannel)

	outputChannel := NewDefaultOutputHandshakeChannel(ib)
	ib.Out = append(ib.Out, outputChannel)

	ib.InData = append(ib.InData, NewDefaultInDataChannel(ib, ib.InputVariables()))
	ib.OutData = append(ib.OutData, NewDefaultOutDataChannel(ib, ib.OutputVariables()))

	// Inner channels -> directions from in/out are handled reversed!
	innerInputChannel := NewOutputHandshakeChannel(ib, "in_req", "in_ack")
	ib.InnerInChannel = append(ib.InnerInChannel, innerInputChannel)

	innerOutputChannel := NewInputHandshakeChannel(ib, "out_req", "out_ack")
	ib.InnerOutChannel = append(ib.InnerOutChannel, innerOutputChannel)

	innerInDataChannel := NewOutDataChannel(ib, ib.InputVariables(), "in_data")
	innerInDataChannel.Connected = true
	ib.InnerInDataChannel = append(ib.InnerInDataChannel, innerInDataChannel)

	innerOutDataChannel := NewInDataChannel(ib, ib.OutputVariables(), "out_data")
	innerOutDataChannel.Connected = true
	ib.InnerOutDataChannel = append(ib.InnerOutDataChannel, innerOutDataChannel)

	infoPrinter.DebugPrintfln("[%s]: Added ifBlock", ib.Name())

	return ib
}

func (ib *IfBlock) AssignBodyComponents(cond *SelectorBlock, thenBody BodyComponentType, elseBody BodyComponentType) {
	ib.cond = cond
	ib.thenBody = thenBody
	ib.elseBody = elseBody

	infoPrinter.DebugPrintfln("[%s]: added bodyComponents: %s, %s and %s", ib.cond.Name(), ib.thenBody.Name(), ib.elseBody.Name())
}

func (ib *IfBlock) Name() string {
	return ib.name
}

func (ib *IfBlock) ComponentStr() string {
	return ib.Name() + `: entity work.IfBlock(` + ib.archName + `)
  generic map(
    DATA_WIDTH => ` + strconv.Itoa(ib.inputVariables.Size) + `
  )
  port map (
	  -- Input Channel
	  in_req => ` + ib.In[0].GetReqSignalName() + `,
	  in_ack => ` + ib.In[0].GetAckSignalName() + `,
	  in_data => ` + ib.InData[0].GetDataSignalName() + `,

	  -- Output Channel
	  out_req => ` + ib.Out[0].GetReqSignalName() + `,
	  out_ack => ` + ib.Out[0].GetAckSignalName() + `,
	  out_data => ` + ib.OutData[0].GetDataSignalName() + `,

	  rst => rst
  );
  `
}

func (ib *IfBlock) Architecture() string {
	if ib.entryFork == nil {
		panic("missing entryFork in ifBlock")
	}
	if ib.cond == nil {
		panic("missing cond in ifBlock")
	}
	if ib.demux == nil {
		panic("missing demux in ifBlock")
	}
	if ib.thenBody == nil {
		panic("missing thenBody in ifBlock")
	}
	if ib.elseBody == nil {
		panic("missing elseBody in ifBlock")
	}
	if ib.merger == nil {
		panic("missing merger in ifBlock")
	}

	ib.createInnerLife()

	handshakeSignalAssignments := ib.getHandshakeSignalAssignments()

	dataSignalAssignments := ib.getDataSignalAssignments()

	signalDefs := ""
	for _, rbp := range ib.RegBlockPairs {
		comp := rbp.Bc

		signalDefs += comp.GetSignalDefs()

		signalDefs += "\n"
	}
	signalDefs += "\n"

	handshakeSignalAssignments += ib.GetInnerHandshakeSignalAssignmentstr()

	componentStr := ib.componentsString()

	signalDefs += ib.GetInnerSignalDefs()
	signalDefs += "\n"

	//handshakeOverwrites := ib.getHandshakeOverwrites(forks, joins)

	ret := "architecture " + ib.archName + " of IfBlock is"
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

	//ret += handshakeOverwrites
	ret += "\n"

	ret += "end process;"

	ret += "\n"

	/* 	ret += ib.entryFork.ComponentStr()
	   	ret += "\n" */
	ret += componentStr

	ret += `end ` + ib.archName + `;
	`
	return ret
}

func (ib *IfBlock) EntityName() string {
	return "ifBlock"
}

func (ib *IfBlock) Entity() string {
	return `LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE work.click_element_library_constants.ALL;
ENTITY ` + ib.EntityName() + ` IS
GENERIC (
	DATA_WIDTH : NATURAL := 8
);
PORT (
	rst : IN STD_LOGIC; -- input channel
	in_req : IN STD_LOGIC;
	in_ack : OUT STD_LOGIC;
	in_data : IN STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0);
	-- Output channel
	out_req : OUT STD_LOGIC;
	out_ack : IN STD_LOGIC;
	out_data : OUT STD_LOGIC_VECTOR(DATA_WIDTH - 1 DOWNTO 0)
);
END ` + ib.EntityName() + `;`

}

func (b *IfBlock) getInnerComponentsSignalDefs() string {
	signalDefs := ""

	//signalDefs += b.entryFork.GetSignalDefs()
	signalDefs += b.cond.GetSignalDefs()
	signalDefs += b.demux.GetSignalDefs()
	signalDefs += b.thenBody.GetSignalDefs()
	signalDefs += b.elseBody.GetSignalDefs()
	signalDefs += b.merger.GetSignalDefs()

	// block signals
	signalDefs += b.GetInnerSignalDefs()

	return signalDefs
}

func (ib *IfBlock) createInnerLife() {
	// Make connections first

	// Inputs to selector
	ib.cond.ConnectHandshake(ib)
	ib.cond.ConnectDataPos(ib, 0, 0)
	ib.cond.ConnectDataPos(ib, 1, 0)

	// Demux
	// Data Path
	ib.demux.ConnectHandshakePos(ib, 0, 0)
	ib.demux.ConnectDataPos(ib, 0, 0)

	// Selector path
	ib.demux.ConnectHandshakePos(ib.cond, 1, 0)
	ib.demux.ConnectDataPos(ib.cond, 1, 0)

	// ThenBody
	ib.thenBody.ConnectHandshakePos(ib.demux, 0, 0)
	ib.thenBody.ConnectDataPos(ib.demux, 0, 0)

	// ElseBody
	ib.elseBody.ConnectHandshakePos(ib.demux, 0, 1)
	ib.elseBody.ConnectDataPos(ib.demux, 0, 1)

	// Merge
	ib.merger.ConnectHandshakePos(ib.thenBody, 0, 0)
	ib.merger.ConnectHandshakePos(ib.elseBody, 1, 0)

	// Merge default inputs
	ib.merger.ConnectDataPos(ib, 0, 0)
	ib.merger.ConnectDataPos(ib, 1, 0)

	ib.merger.ConnectDataPos(ib.thenBody, 0, 0)
	ib.merger.ConnectDataPos(ib.elseBody, 1, 0)

	ib.RegBlockPairs = append(ib.RegBlockPairs, &regBodyPair{Bc: ib.cond})
	ib.RegBlockPairs = append(ib.RegBlockPairs, &regBodyPair{Bc: ib.demux})
	ib.RegBlockPairs = append(ib.RegBlockPairs, &regBodyPair{Bc: ib.thenBody})
	ib.RegBlockPairs = append(ib.RegBlockPairs, &regBodyPair{Bc: ib.elseBody})
	ib.RegBlockPairs = append(ib.RegBlockPairs, &regBodyPair{Bc: ib.merger})

	infoPrinter.DebugPrintfln("[%s]: got componentsStrings", ib.Name())

}
