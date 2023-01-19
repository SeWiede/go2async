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
	demux     *DEMUX
	thenBody  BodyComponentType
	elseBody  BodyComponentType
	merger    *Merge
}

var ifBlockNr = 0

func NewIfBlock(parent BlockType) *IfBlock {
	nr := ifBlockNr
	ifBlockNr++

	name := strings.ToLower(ifBlockPrefix + strconv.Itoa(nr))
	ib := &IfBlock{
		Block: Block{
			BodyComponent: BodyComponent{
				number: nr,

				archName: archPrefix + name,
				/* In: &HandshakeChannel{
					Out: false,
				},
				Out: &HandshakeChannel{
					Req:       name + "_o_req",
					Ack:       name + "_o_ack",
					Data:      name + "_data",
					Out:       true,
					DataWidth: parent.GetCurrentVariableSize(),
				}, */

				parentBlock: parent,

				inputVariables: variable.NewScopedVariables(),
				//outputVariables: variable.NewScopedVariables(),

				predecessors: map[string]BodyComponentType{},
				successors:   map[string]BodyComponentType{},
			},
			ExternalInterfaces: make(map[string]*variable.VariableInfo),
			VariableOwner:      make(map[string]*variableOwner),
		},
	}

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

	infoPrinter.DebugPrintfln("[%s]: Added ifBlock", ib.Name())

	return ib
}

func (ib *IfBlock) AssignBodyComponents(cond *SelectorBlock, thenBody BodyComponentType, elseBody BodyComponentType) {
	ib.cond = cond
	ib.thenBody = thenBody
	ib.elseBody = elseBody
}

func (ib *IfBlock) Name() string {
	return strings.ToLower(ifBlockPrefix + strconv.Itoa(ib.number))
}

func (ib *IfBlock) ComponentStr() string {
	return ib.Name() + `: entity work.IfBlock(` + ib.archName + `)
  generic map(
    DATA_WIDTH => ` + strconv.Itoa(ib.inputVariables.Size) + `
  )
  port map (
    rst => rst,
    in_req => ` + ib.Name() + `_in_req,
    in_ack => ` + ib.Name() + `_in_ack,
    in_data => ` + ib.Name() + `_in_data,
    -- Output channel
    out_req => ` + ib.Name() + `_out_req,
    out_ack => ` + ib.Name() + `_out_ack,
    out_data => ` + ib.Name() + `_out_data 
  );
  `
}

func (ib *IfBlock) Architecture() string {
	ib.entryFork = NewFork(ib.InputVariables().Size)
	ib.demux = NewDEMUX(ib.InputVariables().Size)
	ib.merger = NewMerge(ib.InputVariables().Size)

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

	signalDefs := ib.getSignalDefs()

	dataSignalAssignments := ib.getSignalAssignments()

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

	ret += dataSignalAssignments
	ret += "\n"

	//ret += handshakeOverwrites
	ret += "\n"

	ret += "end process;"

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

func (b *IfBlock) GetVariableLocation(name string) (string, error) {
	sv, err := b.inputVariables.GetVariableInfo(name)
	if err != nil {
		return "", err
	}

	idx := getIndex(sv.Index_)
	totalSize := sv.TotalSize()
	if idx > 0 {
		totalSize = sv.Size_
	}

	position := strconv.Itoa(sv.Position_+totalSize*(idx+1)) + " - 1 downto " + strconv.Itoa(sv.Position_+totalSize*idx)

	return b.Name() + "_in_data(" + position + ")", nil
}

func (b *IfBlock) GetVariable(name string) (*variable.VariableInfo, error) {
	infoPrinter.DebugPrintfln("[%s]: Getting variable %s", b.Name(), name)

	own, ok := b.VariableOwner[name]
	if ok {
		infoPrinter.DebugPrintfln("[%s]: Variable %s's latest owner is %s", b.Name(), name, own.ownerList.lastest.Name())
		return own.vi, nil
	} else {
		if b.Parent() == nil {
			return nil, ErrVariableNotFound(name)
		}

		// Get variable info form parent.
		vi, err := b.Parent().GetVariable(name)
		if err != nil {
			return nil, err
		}

		b.AddPredecessor(b.Parent())
		//prevComps = append(prevComps, b.Parent())

		//nextComps := b.Parent().Successors()
		b.Parent().AddSuccessor(b)

		// Track variables that are coming from outside this block's scope.
		vi, err = b.NewScopeVariable(vi)
		if err != nil {
			return nil, err
		}

		vi.DefinedOnly_ = false

		// New owner of variable in current block is the current block.
		b.Parent().GetVariableOwnerMap()[vi.Name()].ownerList.AddOwner(b)

		b.InputVariables().AddVariable(vi)

		infoPrinter.DebugPrintfln("[%s]: Variable %s is from outside the block's scope. Added to inputs (current size = %d).", b.Name(), name, b.InputVariables().Size)

		return vi, nil
	}

	/* v, err := b.GetScopedVariables().GetVariableInfo(name)
	if err != nil {

	} else {
		infoPrinter.DebugPrintfln("Variable %s's owner is %s", name, own.bc.Name())
		return v, nil
	} */
}
func (b *IfBlock) getDefaultSignalAssignments() string {
	signalAssignments := ""

	// block I/O assignments
	signalAssignments += b.Name() + "_out_req <= in_req;\n"
	signalAssignments += "in_ack <= " + b.Name() + "_out_ack;\n"
	signalAssignments += b.Name() + "_in_data <= in_data;\n"

	signalAssignments += "out_req <= " + b.Name() + "_in_req;\n"
	signalAssignments += b.Name() + "_in_ack <= out_ack;\n"
	signalAssignments += "out_data <= " + b.Name() + "_out_data;\n"
	signalAssignments += "\n"

	infoPrinter.DebugPrintfln("[%s]: assigning block outputs (there are %d)", b.Name(), len(b.OutputVariables().VariableList))

	signalAssignments += b.Name() + "_out_data <= "

	// Block output assignments
	for i, outVar := range b.OutputVariables().VariableList {

		currentInputAssignment := ""
		var err error

		varOwner, ok := b.VariableOwner[outVar.Name()]
		if outVar.Const_ != "" {
			currentInputAssignment += "std_logic_vector(to_signed(" + outVar.Const() + ", " + strconv.Itoa(outVar.TotalSize()) + "))"
		} else {
			if !ok {
				/* inputAssignment += "<???>"
				infoPrinter.DebugPrintfln("%s's input var %s not found!", currentComponent.Name(), input.Name_) */
				panic("Could not find var " + outVar.Name() + "'s owner const is: " + outVar.Const())
			}

			predecessor := varOwner.ownerList.GetLatest()

			currentInputAssignment, err = predecessor.GetVariableLocation(outVar.Name())
			if err != nil {
				panic(err.Error())
			}
		}

		signalAssignments += currentInputAssignment

		if outVar.Size_ != varOwner.vi.Size_ {
			// TODO: disallow implicit casts?
			infoPrinter.DebugPrintfln("[%s] Return variable size mismatch! %d != %d -> casting to outvar size", b.Name(), outVar.Size_, varOwner.vi.Size_)

			signalAssignments += "(" + strconv.Itoa(outVar.Size_) + " - 1 downto 0)"
		}

		if i+1 < len(b.OutputVariables().VariableList) {
			signalAssignments += " & "
		} else {
			signalAssignments += ";\n"
		}

		infoPrinter.DebugPrintfln("[%s]: output %d is %s", b.Name(), i, currentInputAssignment)
	}

	return signalAssignments
}

func (b *IfBlock) getSignalDefs() string {
	signalDefs := ""

	signalDefs += b.getComponentSignalDefs(b.entryFork)
	signalDefs += b.getComponentSignalDefs(b.cond)
	signalDefs += b.getComponentSignalDefs(b.demux)
	signalDefs += b.getComponentSignalDefs(b.thenBody)
	signalDefs += b.getComponentSignalDefs(b.elseBody)
	signalDefs += b.getComponentSignalDefs(b.merger)

	// block signals
	signalDefs += b.getComponentSignalDefs(b)

	return signalDefs
}

func (b *IfBlock) getComponentSignalDefs(currentComponent BodyComponentType) string {
	signalDefs := ""

	if bep, ok := currentComponent.(*BinExprBlock); ok {
		signalDefs += "signal " + currentComponent.Name() + "_in_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_out_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_in_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_out_ack : std_logic;"

		signalDefs += "signal " + currentComponent.Name() + "_x : std_logic_vector(" + strconv.Itoa(bep.GetXTotalSize()) + "- 1 downto 0) := (others => '0');"
		signalDefs += "signal " + currentComponent.Name() + "_y : std_logic_vector(" + strconv.Itoa(bep.GetYTotalSize()) + "- 1 downto 0) := (others => '0');"
		signalDefs += "signal " + currentComponent.Name() + "_result : std_logic_vector(" + strconv.Itoa(bep.GetRTotalSize()) + "- 1 downto 0);"

	} else if sb, ok := currentComponent.(*SelectorBlock); ok {
		signalDefs += "signal " + currentComponent.Name() + "_in_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_out_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_in_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_out_ack : std_logic;"

		signalDefs += "signal " + currentComponent.Name() + "_x : std_logic_vector(" + strconv.Itoa(sb.GetXTotalSize()) + "- 1 downto 0) := (others => '0');"
		signalDefs += "signal " + currentComponent.Name() + "_y : std_logic_vector(" + strconv.Itoa(sb.GetYTotalSize()) + "- 1 downto 0) := (others => '0');"
		signalDefs += "signal " + currentComponent.Name() + "_selector : std_logic_vector(0 downto 0);"
	} else if f, ok := currentComponent.(*Fork); ok {
		/*
		   inA_req => ` + f.Name() + `_inA_req,
		   inA_ack => ` + f.Name() + `_inA_ack,
		   inA_data => ` + f.Name() + `_inA_data,

		   -- Output Channel 1
		   outB_req => ` + f.Name() + `_outB_req,
		   outB_ack => ` + f.Name() + `_outB_ack,
		   outB_data => ` + f.Name() + `_out_B_data,

		   -- Output Channel 2
		   outC_req => ` + f.Name() + `_outC_req,
		   outC_ack => ` + f.Name() + `_outC_ack,
		   outC_data => ` + f.Name() + `_outC_data,
		*/

		signalDefs += "signal " + currentComponent.Name() + "_inA_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_inA_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_inA_data : std_logic_logic(" + strconv.Itoa(f.DataWidth) + " - 1 downto 0);"

		signalDefs += "signal " + currentComponent.Name() + "_outB_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outB_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outB_data : std_logic_logic(" + strconv.Itoa(f.DataWidth) + " - 1 downto 0);"

		signalDefs += "signal " + currentComponent.Name() + "_outC_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outC_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outC_data : std_logic_logic(" + strconv.Itoa(f.DataWidth) + " - 1 downto 0);"
	} else if m, ok := currentComponent.(*Merge); ok {
		signalDefs += "signal " + currentComponent.Name() + "_inA_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_inA_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_inA_data : std_logic_logic(" + strconv.Itoa(m.DataWidth) + " - 1 downto 0);"

		signalDefs += "signal " + currentComponent.Name() + "_inB_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_inB_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_inB_data : std_logic_logic(" + strconv.Itoa(m.DataWidth) + " - 1 downto 0);"

		signalDefs += "signal " + currentComponent.Name() + "_outC_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outC_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outC_data : std_logic_logic(" + strconv.Itoa(m.DataWidth) + " - 1 downto 0);"
	} else if d, ok := currentComponent.(*DEMUX); ok {
		signalDefs += "signal " + currentComponent.Name() + "_inA_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_inA_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_inA_data : std_logic_logic(" + strconv.Itoa(d.DataWidth) + " - 1 downto 0);"

		signalDefs += "signal " + currentComponent.Name() + "_outB_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outB_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outB_data : std_logic_logic(" + strconv.Itoa(d.DataWidth) + " - 1 downto 0);"

		signalDefs += "signal " + currentComponent.Name() + "_outC_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outC_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_outC_data : std_logic_logic(" + strconv.Itoa(d.DataWidth) + " - 1 downto 0);"

		signalDefs += "signal " + currentComponent.Name() + "_inSel_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_inSel_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_selector : std_logic_logic(0 downto 0);"

	} else {
		signalDefs += "signal " + currentComponent.Name() + "_in_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_out_req : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_in_ack : std_logic;"
		signalDefs += "signal " + currentComponent.Name() + "_out_ack : std_logic;"

		signalDefs += "signal " + currentComponent.Name() + "_in_data : std_logic_vector(" + strconv.Itoa(currentComponent.InputVariables().Size) + "- 1 downto 0);"
		signalDefs += "signal " + currentComponent.Name() + "_out_data : std_logic_vector(" + strconv.Itoa(currentComponent.OutputVariables().Size) + "- 1 downto 0);"
	}

	signalDefs += "\n"

	return signalDefs
}

func (b *IfBlock) getSignalAssignments() string {
	signalAssignments := ""

	// Block defaults
	signalAssignments += b.Name() + "_out_req <= in_req;\n"
	signalAssignments += "in_ack <= " + b.Name() + "_out_ack;\n"
	signalAssignments += b.Name() + "_in_data <= in_data;\n"

	signalAssignments += "out_req <= " + b.Name() + "_in_req;\n"
	signalAssignments += b.Name() + "_in_ack <= out_ack;\n"
	signalAssignments += "out_data <= " + b.Name() + "_out_data;\n"
	signalAssignments += "\n"

	// Selector
	signalAssignments += b.cond.Name() + "_in_req <= _in_req;"
	signalAssignments += b.cond.Name() + "_in_ack <= _in_ack;"

	signalAssignments += b.cond.Name() + "_out_req <= _out_req;"
	signalAssignments += b.cond.Name() + "_out_ack <= _out_ack;"

	// DEMUX

	// then

	// else

	// merge

	// block output

	/* signalDefs += "signal " + currentComponent.Name() + "_out_ack"

	signalDefs += "signal " + currentComponent.Name() + "_x : std_logic_vector(" + strconv.Itoa(sb.GetXTotalSize()) + "- 1 downto 0) := (others => '0');"
	signalDefs += "signal " + currentComponent.Name() + "_y : std_logic_vector(" + strconv.Itoa(sb.GetYTotalSize()) + "- 1 downto 0) := (others => '0');"
	signalDefs += "signal " + currentComponent.Name() + "_selector : std_logic_vector(0 downto 0);" */

	return signalAssignments
}
