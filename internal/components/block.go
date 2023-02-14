package components

import (
	"errors"
	infoPrinter "go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"strconv"
	"strings"
)

const blockPrefix = "B_"
const defaultBlockEntityName = "BlockC"

func ErrVariableNotFound(v string) error {
	return errors.New("Variable '" + v + "' not found.")
}

type regBodyPair struct {
	Reg *Reg
	Bc  BodyComponentType
}

type Block struct {
	BodyComponent
	Nr int

	TopLevel       bool
	BodyComponents []BodyComponentType

	VariableOwner      map[string]*variableOwner
	ExternalInterfaces map[string]*variable.VariableInfo

	RegBlockPairs []*regBodyPair

	// Inner connection
	InnerInChannel  []*HandshakeChannel
	InnerOutChannel []*HandshakeChannel

	InnerInDataChannel  []*DataChannel
	InnerOutDataChannel []*DataChannel

	KeepVariableOwnership bool
}

func NewScopedVariables() *variable.ScopedVariables {
	return &variable.ScopedVariables{
		Variables:    make(map[string]*variable.VariableInfo),
		VariableList: []*variable.VariableInfo{},
		Size:         0,
		ParamPos:     0,
	}
}

var blockNr = 0

func NewBlock(toplevel bool, keepVariableOwnership bool, parent BlockType) *Block {
	nr := blockNr
	blockNr++

	name := strings.ToLower(blockPrefix + strconv.Itoa(nr))

	b := &Block{
		BodyComponent: BodyComponent{
			name: strings.ToLower(blockPrefix + strconv.Itoa(nr)),

			number:   nr,
			archName: archPrefix + name,
			/* In: &HandshakeChannel{
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
			}, */
			parentBlock: parent,

			inputVariables: NewScopedVariables(),
			//outputVariables: NewScopedVariables(),

			predecessors: map[string]BodyComponentType{},
			successors:   map[string]BodyComponentType{},

			isBlock: true,
		},

		ExternalInterfaces: make(map[string]*variable.VariableInfo),
		VariableOwner:      make(map[string]*variableOwner),

		TopLevel:              toplevel,
		KeepVariableOwnership: keepVariableOwnership,
	}

	inputChannel := NewDefaultInputHandshakeChannel(b)
	b.In = append(b.In, inputChannel)

	outputChannel := NewDefaultOutputHandshakeChannel(b)
	b.Out = append(b.Out, outputChannel)

	b.InData = append(b.InData, NewDefaultInDataChannel(b, b.InputVariables()))
	b.OutData = append(b.OutData, NewDefaultOutDataChannel(b, b.OutputVariables()))

	// Inner channels -> directions from in/out are handled reversed!
	innerInputChannel := NewOutputHandshakeChannel(b, "in_req", "in_ack")
	b.InnerInChannel = append(b.InnerInChannel, innerInputChannel)

	innerOutputChannel := NewInputHandshakeChannel(b, "out_req", "out_ack")
	b.InnerOutChannel = append(b.InnerOutChannel, innerOutputChannel)

	innerInDataChannel := NewOutDataChannel(b, b.InputVariables(), "in_data")
	innerInDataChannel.Connected = true
	b.InnerInDataChannel = append(b.InnerInDataChannel, innerInDataChannel)

	innerOutDataChannel := NewInDataChannel(b, b.OutputVariables(), "out_data")
	innerOutDataChannel.Connected = true
	b.InnerOutDataChannel = append(b.InnerOutDataChannel, innerOutDataChannel)

	return b
}

func NewParamDummyBlock(params map[string]*variable.VariableInfo) *Block {
	ret := &Block{
		ExternalInterfaces: make(map[string]*variable.VariableInfo),
		VariableOwner:      make(map[string]*variableOwner),
	}

	ret.parentBlock = nil
	ret.inputVariables = &variable.ScopedVariables{
		Variables: params,
	}

	ret.inputVariables.Size = 0
	for _, v := range params {
		ret.inputVariables.Size += v.Len_ * v.Size_
	}

	ret.name = "ParamDummyBlock"
	ret.archName = "DummyBlock"

	ret.predecessors = map[string]BodyComponentType{}
	ret.successors = map[string]BodyComponentType{}

	return ret
}

func (b *Block) Name() string {
	return b.name
}

func (b *Block) AddComponent(bodyComponent BodyComponentType) {
	b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{Bc: bodyComponent})
	/* b.Out = bodyComponent.OutChannel()

	// TODO: explore top-level behaviour!
	if b.TopLevel *false {
		newreg := NewReg(bodyComponent.GetVariableSize(), false, "0")
		newreg.Out.Connect(bodyComponent.InChannel())

		if len(b.RegBlockPairs) == 0 {
			*newreg.InChannel() = *b.In
		} else {
			b.Out = bodyComponent.OutChannel()
			b.RegBlockPairs[len(b.RegBlockPairs)-1].Bc.OutChannel().Connect(newreg.InChannel())
		}

		b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{newreg, bodyComponent})
	} else {
		b.Out = bodyComponent.OutChannel()

		if len(b.RegBlockPairs) == 0 {
			entryIn := &HandshakeChannel{
				Req:  "in_req",
				Ack:  "in_ack",
				Data: "in_data",
				Out:  true,
			}
			*bodyComponent.InChannel() = *entryIn
		} else {
			b.Out = bodyComponent.OutChannel()
			b.RegBlockPairs[len(b.RegBlockPairs)-1].Bc.OutChannel().Connect(bodyComponent.InChannel())
		}

		b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{Bc: bodyComponent})
	} */
}

func (b *Block) EntityName() string {
	return defaultBlockEntityName + "_" + strconv.Itoa(len(b.ExternalInterfaces))
}

func (b *Block) ComponentStr() string {
	name := b.Name()

	dataInWidthStr := strconv.Itoa(b.InputVariables().Size)
	dataOutWidthStr := strconv.Itoa(b.OutputVariables().Size)

	externalInterfacesStr := ``
	externalIntferacesGenericsStr := ``
	comma := ``
	if len(b.ExternalInterfaces) > 0 {
		comma = `,`
	}

	i := 0
	for fName, _ := range b.ExternalInterfaces {
		externalIntferacesGenericsStr += fName + `_IN_DATA_WIDTH => ` + fName + "_IN_DATA_WIDTH,\n"
		externalIntferacesGenericsStr += fName + `_OUT_DATA_WIDTH => ` + fName + `_OUT_DATA_WIDTH`

		externalInterfacesStr += `-- Interface for ` + fName
		externalInterfacesStr += `
		-- Input channel
		` + fName + `_in_data  => ` + fName + `_in_data,
		` + fName + `_in_req => ` + fName + `_in_req,
		` + fName + `_in_ack => ` + fName + `_in_ack,
		-- Output channel
		` + fName + `_out_data => ` + fName + `_out_data,
		` + fName + `_out_req => ` + fName + `_out_req,
		` + fName + `_out_ack => ` + fName + `_out_ack`

		if i != len(b.ExternalInterfaces)-1 {
			externalInterfacesStr += ",\n"
			externalIntferacesGenericsStr += ",\n"
		}
		i++
	}

	return name + `: entity work.` + b.EntityName() + `(` + b.archName + `)
  generic map(
   DATA_IN_WIDTH => ` + dataInWidthStr + `,
   DATA_OUT_WIDTH => ` + dataOutWidthStr + comma + `
   ` + externalIntferacesGenericsStr + `
  )
  port map (
	  -- Input channel
	  in_req => ` + b.In[0].GetReqSignalName() + `,
	  in_ack => ` + b.In[0].GetAckSignalName() + `,
	  in_data => ` + b.InData[0].GetDataSignalName() + `,

	  -- Output channel
	  out_req => ` + b.Out[0].GetReqSignalName() + `,
	  out_ack => ` + b.Out[0].GetAckSignalName() + `,
	  out_data => ` + b.OutData[0].GetDataSignalName() + `,

	  rst => rst` + comma + `
	-- External interfaces
	` + externalInterfacesStr + `
  );
  `
	//in_data => std_logic_vector(resize(unsigned(` + b.In.Data + `), ` + strconv.Itoa(b.GetVariableSize()) + `)),
}

func (b *Block) ioChannels() string {
	ret := ""
	if len(b.RegBlockPairs) == 0 {
		ret += "out_req <= in_req; \n"
		ret += "in_ack <= out_ack; \n"
		ret += "out_data <= in_data; \n"
	} else {
		/* ret += "out_req <= " + b.OutChannel().Req + "; \n"
		ret += b.OutChannel().Ack + " <= out_ack; \n"
		ret += "out_data <= std_logic_vector(resize(unsigned(" + b.OutChannel().Data + "), out_data'length)); \n" */
	}

	return ret
}

func (b *Block) componentsString() string {
	ret := ""
	for _, c := range b.RegBlockPairs {
		if c.Reg != nil {
			ret += c.Reg.ComponentStr() + "\n"
		}
		ret += c.Bc.ComponentStr() + "\n"

		for _, in := range c.Bc.InChannels() {
			ret += in.join.ComponentStr()
		}

		for _, out := range c.Bc.OutChannels() {
			ret += out.fork.ComponentStr()
		}
	}

	for _, in := range b.InnerInChannel {
		ret += in.fork.ComponentStr()
	}

	for _, out := range b.InnerOutChannel {
		ret += out.join.ComponentStr()
	}

	return ret
}

func (b *Block) Entity() string {
	infoPrinter.DebugPrintf("Generating unique block entity '%s'\n", b.EntityName())

	externalInterfacesStr := ``
	externalIntferacesGenericsStr := ``
	semiColon := ``
	if len(b.ExternalInterfaces) > 0 {
		semiColon = `;`
	}

	i := 0
	for fName, _ := range b.ExternalInterfaces {
		externalIntferacesGenericsStr += fName + "_IN_DATA_WIDTH : NATURAL := 8;\n"
		externalIntferacesGenericsStr += fName + `_OUT_DATA_WIDTH : NATURAL := 8`

		externalInterfacesStr += `-- Interface for ` + fName
		externalInterfacesStr += `
		-- Input channel
		` + fName + `_in_data : OUT STD_LOGIC_VECTOR(` + fName + `_IN_DATA_WIDTH - 1 DOWNTO 0);
		` + fName + `_in_req : OUT STD_LOGIC;
		` + fName + `_in_ack : IN STD_LOGIC;
		-- Output channel
		` + fName + `_out_data : IN STD_LOGIC_VECTOR(` + fName + `_OUT_DATA_WIDTH - 1 DOWNTO 0);
		` + fName + `_out_req : IN STD_LOGIC;
		` + fName + `_out_ack : OUT STD_LOGIC`

		if i != len(b.ExternalInterfaces)-1 {
			externalIntferacesGenericsStr += ";\n"
			externalInterfacesStr += ";\n"
		}
		i++
	}

	ret := `LIBRARY IEEE;
	USE IEEE.STD_LOGIC_1164.ALL;
	USE ieee.std_logic_unsigned.ALL;
	USE ieee.numeric_std.ALL;
	USE work.click_element_library_constants.ALL;
	
	ENTITY ` + b.EntityName() + ` IS
	  GENERIC (
		DATA_IN_WIDTH : NATURAL := 8;
		DATA_OUT_WIDTH : NATURAL := 8` + semiColon + `
		` + externalIntferacesGenericsStr + `
	  );
	  PORT (
		rst : IN STD_LOGIC;
		-- Input channel
		in_data : IN STD_LOGIC_VECTOR(DATA_IN_WIDTH - 1 DOWNTO 0);
		in_req : IN STD_LOGIC;
		in_ack : OUT STD_LOGIC;
		-- Output channel
		out_data : OUT STD_LOGIC_VECTOR(DATA_OUT_WIDTH - 1 DOWNTO 0);
		out_req : OUT STD_LOGIC;
		out_ack : IN STD_LOGIC` + semiColon + `

		-- External interfaces
		` + externalInterfacesStr +
		`
	  );
	  END ` + b.EntityName() + `;`

	return ret
}

func (b *Block) getHandshakeSignalAssignments() string {
	handShakeAssignments := ""

	for _, rbp := range b.RegBlockPairs {
		comp := rbp.Bc

		handShakeAssignments += "-- Handshake signals assignments for " + comp.Name()
		handShakeAssignments += "\n"

		handShakeAssignments += comp.GetHandshakeSignalAssigmentStr()

		handShakeAssignments += "-------------------------------"
		handShakeAssignments += "\n"
		handShakeAssignments += "\n"
	}
	handShakeAssignments += "\n"

	return handShakeAssignments
}

func (b *Block) getDataSignalAssignments() string {
	dataSignalAssignments := ""

	for _, rbp := range b.RegBlockPairs {
		comp := rbp.Bc

		dataSignalAssignments += "-- Data signal assignments for " + comp.Name()
		dataSignalAssignments += "\n"

		infoPrinter.DebugPrintfln("[%s]: data signal assignments for "+comp.Name(), comp.Name())
		dataSignalAssignments += comp.GetDataSignalAssigmentStr()

		dataSignalAssignments += "-------------------------------"
		dataSignalAssignments += "\n"
		dataSignalAssignments += "\n"
	}
	dataSignalAssignments += "\n"

	return dataSignalAssignments
}

func (b *Block) Architecture() string {
	// Determine how many forks and joins are needed

	//signalDefs := b.signalDefs()
	/* dataSignalAssignments := b.getDefaultSignalAssignments()

	signalDefs, forks, joins := b.getSignalDefsJoinsAndForks()

	handshakeOverwrites := b.getHandshakeOverwrites(forks, joins) */

	signalDefs := ""

	handShakeAssignments := b.getHandshakeSignalAssignments()
	dataSignalAssignments := b.getDataSignalAssignments()

	for _, rbp := range b.RegBlockPairs {
		comp := rbp.Bc

		signalDefs += comp.GetSignalDefs()

		signalDefs += "\n"
	}
	signalDefs += "\n"

	// Handle block at the end since there might be some new default connections to the parent

	handShakeAssignments += "------ Block assignments"
	handShakeAssignments += "\n"
	handShakeAssignments += b.GetInnerHandshakeSignalAssignmentstr()
	handShakeAssignments += "------------------------"
	handShakeAssignments += "\n"
	handShakeAssignments += "\n"

	signalDefs += b.GetInnerSignalDefs()
	signalDefs += "\n"

	handshakeOverwrites := ""

	ret := `architecture ` + b.archName + ` of ` + b.EntityName() + ` is
	`
	ret += signalDefs

	ret += "\n"
	ret += "begin"
	ret += "\n"

	//ret += b.ioChannels()

	ret += "signalAssignments: process(all)"
	ret += "\n"
	ret += "begin"
	ret += "\n"

	ret += handShakeAssignments
	ret += "\n"

	ret += "\n"

	ret += dataSignalAssignments
	ret += "\n"

	ret += handshakeOverwrites
	ret += "\n"

	ret += "end process;"
	ret += "\n"

	ret += "\n"

	ret += b.componentsString()

	ret += `end ` + b.archName + `;
	`
	return ret
}

func (b *Block) GetScopedVariables() *variable.ScopedVariables {
	return b.inputVariables
}

func (b *Block) GetCurrentVariableSize() int {
	return b.inputVariables.Size
}

func (b *Block) NewScopeVariable(vdef variable.VariableDef) (*variable.VariableInfo, error) {
	infoPrinter.DebugPrintfln("[%s]: Adding variable %s", b.Name(), vdef.Name())

	/* 	vi, err := b.GetScopedVariables().AddVariable(decl)
	   	if err != nil {
	   		return nil, err
	   	} */

	vi, err := variable.FromDef(vdef)
	if err != nil {
		return nil, err
	}

	b.VariableOwner[vdef.Name()] = &variableOwner{
		ownerList: NewOwnerList(b),
		vi:        vi,
	}

	return vi, nil
}

func (b *Block) GetVariable(varName string) (*variable.VariableInfo, error) {
	infoPrinter.DebugPrintfln("[%s]: Getting variable %s", b.Name(), varName)

	own, ok := b.VariableOwner[varName]
	if ok {
		infoPrinter.DebugPrintfln("Variable %s's latest owner is %s", varName, own.ownerList.lastest.Name())
		return own.vi, nil
	} else {
		if b.Parent() == nil {
			return nil, ErrVariableNotFound(varName)
		}

		// Get variable info form parent.
		vi, err := b.Parent().GetVariable(varName)
		if err != nil {
			return nil, err
		}

		// Track variables that are coming from outside this block's scope.
		vi, err = b.NewScopeVariable(vi)
		if err != nil {
			return nil, err
		}

		vi.DefinedOnly_ = false

		if !b.KeepVariableOwnership {
			// Skip top block
			if b.Parent().Parent() != nil {
				// Connect with owner of variable
				ownX, ok := b.Parent().GetVariableOwnerMap()[varName]
				if !ok {
					// Shouldn't happen
					return nil, errors.New("No owner for X operator")
				}

				latestOwner := ownX.ownerList.lastest
				ownX.ownerList.AddSuccessorToLatest(b)

				// Claim ownership of used variable in parent
				ownX.ownerList.AddOwner(b)

				b.AddPredecessor(latestOwner)
				//prevComps = append(prevComps, b.Parent())

				//nextComps := b.Parent().Successors()
				latestOwner.AddSuccessor(b)

				b.ConnectHandshake(latestOwner)
				b.ConnectData(latestOwner)

				infoPrinter.DebugPrintfln("[%s]: connected to %s and got ownerShip of var %s", b.Name(), latestOwner.Name(), varName)

				// TODO: OWNERSHIP HANDLING
			}

		} else {
			// Expect connection later

			infoPrinter.DebugPrintfln("[%s]: keeping variable ownership and wait for later connections!", b.Name())
		}

		/* // New owner of variable in current block is the current block.
		b.VariableOwner[vi.Name()] = &variableOwner{
			ownerList: NewOwnerList(b),
			vi:        vi,
		}
		*/
		b.InputVariables().AddVariable(vi)

		//b.AddInputVariable(vi)

		infoPrinter.DebugPrintfln("[%s] Variable %s is from outside the block's scope. Added to inputs and owners (current size = %d).", b.Name(), varName, b.InputVariables().Size)

		return vi, nil
	}

	/* v, err := b.GetScopedVariables().GetVariableInfo(name)
	if err != nil {

	} else {
		infoPrinter.DebugPrintfln("Variable %s's owner is %s", name, own.bc.Name())
		return v, nil
	} */
}

func (b *Block) AddFunctionInterface(f *variable.VariableInfo) error {
	if _, ok := b.ExternalInterfaces[f.Name_]; ok {
		return errors.New("Functionpointer already decalred")
	}

	b.ExternalInterfaces[f.Name_] = f.Copy()

	infoPrinter.DebugPrintf("Added func %s to block %s\n", f.Name_, b.archName)

	return nil
}

func (b *Block) GetAndAssignFunctionInterface(fname string) (*variable.VariableInfo, error) {
	if f, ok := b.ExternalInterfaces[fname]; ok {
		return f, nil
	}

	infoPrinter.DebugPrintf("Function '%s' not found at block %s - searching parent\n", fname, b.archName)

	if b.parentBlock != nil {
		f, err := b.parentBlock.GetAndAssignFunctionInterface(fname)
		if err == nil {
			// parent-stack had the function defined: add the interface to block
			fiCopy := f.Copy()
			b.ExternalInterfaces[f.Name_] = fiCopy

			infoPrinter.DebugPrintf("Found function '%s' on parent stack and registerd function '%s' at block %s\n", f.Name_, f.Name_, b.archName)

			return fiCopy, nil
		}
	}

	return nil, errors.New("No function defined for " + fname)
}

func (b *Block) GetVariableLocationFromSelf(name string) (string, error) {

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

	return "in_data(" + position + ")", nil
}

func (b *Block) GetVariableLocation(name string) (string, error) {

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

	return b.Name() + "_out_data(" + position + ")", nil
}

func (b *Block) getComponentSignalDefJoinsAndForks(currentComponent BodyComponentType) (string, []*MultiHsFork, []*MultiHsJoin) {
	signalDefs := ""
	joins := []*MultiHsJoin{}
	forks := []*MultiHsFork{}

	signalDefs += currentComponent.GetSignalDefs()

	signalDefs += "\n"

	//invs := bc.InputVariables()

	joinNeeded := false
	forkNeeded := false

	predecessors := currentComponent.Predecessors()
	successors := currentComponent.Successors()

	infoPrinter.DebugPrintfln("%s's child %s; %d predecessors %d successors", b.Name(), currentComponent.Name(), len(predecessors), len(successors))

	if len(predecessors) > 1 {
		joinNeeded = true
	}
	if len(successors) > 1 {
		forkNeeded = true
	}

	if joinNeeded {
		// Add Join to block
		newJoin, _ := NewMultiHsJoin(predecessors, currentComponent)
		joins = append(joins, newJoin)

		b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{
			Bc: newJoin,
		})

		signalDefs += newJoin.GetSignalDefs()

		signalDefs += "\n"
	}

	if forkNeeded {
		// Add Fork to block
		newFork, _ := NewMultiHsFork(successors, currentComponent)
		forks = append(forks, newFork)

		b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{
			Bc: newFork,
		})

		signalDefs += newFork.GetSignalDefs()

		signalDefs += "\n"
	}

	infoPrinter.DebugPrintfln("%s: forks (%t) joins (%t)", currentComponent.Name(), forkNeeded, joinNeeded)

	return signalDefs, forks, joins
}

func (b *Block) getSignalDefsJoinsAndForks() (string, []*MultiHsFork, []*MultiHsJoin) {
	forks := []*MultiHsFork{}
	joins := []*MultiHsJoin{}
	signalDefs := ""

	openEndComps := map[string]BodyComponentType{}

	for i, rbp := range b.RegBlockPairs {
		if rbp.Reg != nil {
			panic("pipeline regs not supported yet")
		}

		infoPrinter.DebugPrintfln("[%s/%s]: processing %d. child", b.Name(), rbp.Bc.Name(), i+1)

		sd, f, j := b.getComponentSignalDefJoinsAndForks(rbp.Bc)

		if len(rbp.Bc.Successors()) == 0 {
			// Track comps with no successors for end-of-block join
			openEndComps[rbp.Bc.Name()] = rbp.Bc
		}

		signalDefs += sd
		forks = append(forks, f...)
		joins = append(joins, j...)
	}

	// block signals
	infoPrinter.DebugPrintfln("[%s]: processing block itself", b.Name())
	sd, f, j := b.getComponentSignalDefJoinsAndForks(b)

	signalDefs += sd
	forks = append(forks, f...)
	joins = append(joins, j...)

	if len(openEndComps) > 1 {
		// Add end-of-block join
		endJoin, err := NewMultiHsJoin(openEndComps, b)
		if err != nil {
			panic("error while building endJoin")
		}

		signalDefs += endJoin.GetSignalDefs()

		signalDefs += "\n"

		joins = append(joins, endJoin)

		b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{
			Bc: endJoin,
		})
	}

	return signalDefs, forks, joins
}

func (b *Block) getDefaultSignalAssignments() string {
	signalAssignments := ""

	// block I/O assignments
	signalAssignments += b.Name() + "_out_req <= in_req;\n"
	signalAssignments += "in_ack <= " + b.Name() + "_out_ack;\n"
	signalAssignments += "out_req <= " + b.Name() + "_in_req;\n"
	signalAssignments += b.Name() + "_in_ack <= out_ack;\n"

	/* if b.inputVariables == b.outputVariables {
		// if there were no outputOverwrites
		signalAssignments += b.Name() + "_out_data <= in_data;\n"

		signalAssignments += "out_data <= " + b.Name() + "_in_data;\n"
	} */
	signalAssignments += "\n"

	for _, rbp := range b.RegBlockPairs {
		currentComponent := rbp.Bc

		infoPrinter.DebugPrintfln("[%s]: getting default signalassignment of child %s", b.Name(), currentComponent.Name())

		// Signal assigments

		// signalAssignments :=
		/* signalAssignments += currentComponent.Name() + "_in_req <= " + owner.bc.Name() + "_out_req;"
		signalAssignments += currentComponent.Name() + "<= " + owner.bc.Name() + "_out_req;"
		signalAssignments += currentComponent.Name() + "<= " + owner.bc.Name() + "_in_ack;"
		signalAssignments += currentComponent.Name() + "<= " + owner.bc.Name() + "_out_ack;" */

		// gather all inputs

		inputAssignment := ""

		recipient := currentComponent.Name() + "_in_data"

		if _, ok := currentComponent.(*BinExprBlock); ok {

		} else if _, ok := currentComponent.(*SelectorBlock); ok {

		} else {
			signalAssignments += recipient + " <= "
		}

		infoPrinter.DebugPrintfln("%s's has %d inputs", currentComponent.Name(), len(currentComponent.InputVariables().VariableList))

		if len(currentComponent.InputVariables().VariableList) <= 0 {
			panic("No input variables for component " + currentComponent.Name())
		}

		// default predecessor and successor is the parent block.
		for i, input := range currentComponent.InputVariables().VariableList {
			currentInputAssignment := b.getInputStr(currentComponent, input)

			switch currentComponent.(type) {
			case *BinExprBlock, *SelectorBlock:
				if i == 0 {
					recipient = currentComponent.Name() + "_x"
					signalAssignments += recipient + " <= " + currentInputAssignment
				} else if i == 1 {
					recipient = currentComponent.Name() + "_y"
					signalAssignments += recipient + " <= " + currentInputAssignment
				}
				signalAssignments += ";\n"
			default:
				inputAssignment += currentInputAssignment

				if i+1 < len(currentComponent.InputVariables().VariableList) {
					inputAssignment += " & "
				} else {
					signalAssignments += inputAssignment
					signalAssignments += ";\n"
				}
			}

			infoPrinter.DebugPrintfln("%s's input var %s comes from %s therefore: %s", currentComponent.Name(), input.Name_, currentInputAssignment, inputAssignment)
		}

		// In case of fork/joins some of these values are overwritten later

		predecessor := b.Name()
		for _, pred := range currentComponent.Predecessors() {
			predecessor = pred.Name()
			break
		}

		successor := b.Name()
		for _, succ := range currentComponent.Successors() {
			successor = succ.Name()
			break
		}

		signalAssignments += currentComponent.Name() + "_in_req <= " + predecessor + "_out_req;"
		signalAssignments += predecessor + "_out_ack <= " + currentComponent.Name() + "_in_ack;"

		signalAssignments += successor + "_in_req <= " + currentComponent.Name() + "_out_req;"
		signalAssignments += currentComponent.Name() + "_out_ack <= " + successor + "_in_ack;"
		signalAssignments += "\n"
	}

	infoPrinter.DebugPrintfln("[%s]: assigning block outputs (there are %d)", b.Name(), len(b.OutputVariables().VariableList))

	signalAssignments += "out_data <= "

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

func (b *Block) getHandshakeOverwrites(forks []*MultiHsFork, joins []*MultiHsJoin) string {
	signalOverwrites := ""

	for _, fork := range forks {
		// 1 in_req, more out_req

		signalOverwrites += fork.Name() + "_in_req <= " + fork.Sender.Name() + "_out_req;\n"
		signalOverwrites += fork.Sender.Name() + "_out_ack <= " + fork.Name() + "_in_ack;\n"

		forkOutAck := fork.Name() + "_out_ack <= "

		// Order does not matter
		i := 0
		for _, receiver := range fork.Receivers {
			istr := strconv.Itoa(i)
			signalOverwrites += receiver.Name() + "_in_req <= " + fork.Name() + "_out_req(" + istr + ");\n"

			forkOutAck += receiver.Name() + "_in_ack"

			if i+1 < len(fork.Receivers) {
				forkOutAck += " & "
			}

			i++
		}

		signalOverwrites += forkOutAck + ";\n"
	}

	for _, join := range joins {
		// more in_req, 1 out_req

		signalOverwrites += join.Name() + "_out_ack <= " + join.Receiver.Name() + "_in_ack;\n"
		signalOverwrites += join.Receiver.Name() + "_in_req <= " + join.Name() + "_out_req;\n"

		joinInReq := join.Name() + "_in_req <= "

		// Order does not matter
		i := 0
		for _, sender := range join.Senders {
			istr := strconv.Itoa(i)
			signalOverwrites += sender.Name() + "_out_ack <= " + join.Name() + "_in_ack(" + istr + ");\n"

			joinInReq += sender.Name() + "_out_req"

			if i+1 < len(join.Senders) {
				joinInReq += " & "
			}

			i++
		}

		signalOverwrites += joinInReq + ";\n"
	}

	return signalOverwrites
}

func (b *Block) SetOutput(vis []*variable.VariableInfo) error {
	infoPrinter.DebugPrintfln("[%s]: Overwriting implicit output variables", b.Name())

	b.outputVariables = NewScopedVariables()

	for _, vi := range vis {
		_, err := b.AddOutputVariable(vi)
		if err != nil {
			return err
		}

		infoPrinter.DebugPrintfln("[%s]: Added explicit output var %s", b.Name(), vi.Name_)
	}

	b.InnerOutDataChannel[0].variables = b.OutputVariables()

	return nil
}

func (b *Block) getInputStr(currentComponent BodyComponentType, input *variable.VariableInfo) string {
	currentInputAssignment := ""
	if input.Const_ != "" {
		currentInputAssignment += "std_logic_vector(to_signed(" + input.Const_ + ", "

		currentInputAssignment += strconv.Itoa(input.TotalSize())

		currentInputAssignment += "))"
	} else {
		varOwner, ok := b.VariableOwner[input.Name_]
		if !ok {
			panic("Could not find var " + input.Name_ + "'s owner const is: " + input.Const_)
		}

		predecessor, err := varOwner.ownerList.GetOwnerOf(currentComponent)
		if err != nil {
			panic("[" + currentComponent.Name() + "]: There was no predecessor for variable " + input.Name() + "; error: " + err.Error())
		}

		if predecessor == b {
			currentInputAssignment, err = b.GetVariableLocationFromSelf(input.Name_)
			if err != nil {
				panic(err.Error())
			}
		} else {
			currentInputAssignment, err = predecessor.GetVariableLocation(input.Name_)
			if err != nil {
				panic(err.Error())
			}
		}
	}

	return currentInputAssignment
}

func (b *Block) GetInnerSignalDefs() string {
	signalDefs := ""

	for _, in := range b.InnerInChannel {
		signalDefs += in.SignalDefsForkJoin()
	}
	for _, out := range b.InnerOutChannel {
		signalDefs += out.SignalDefsForkJoin()
	}

	return signalDefs
}

func (b *Block) GetInnerHandshakeSignalAssignmentstr() string {
	handShakeAssignments := ""

	for _, in := range b.InnerInChannel {
		handShakeAssignments += in.GetSignalAssigmentStr()

		handShakeAssignments += "\n"
	}

	handShakeAssignments += "\n"

	for _, out := range b.InnerOutChannel {
		handShakeAssignments += out.GetSignalAssigmentStr()

		handShakeAssignments += "\n"
	}
	return handShakeAssignments
}

func (b *Block) GetInnerIn() *HandshakeChannel {
	return b.InnerInChannel[0]
}

func (b *Block) GetInnerOut() *HandshakeChannel {
	return b.InnerOutChannel[0]
}

func (b *Block) GetInnerInData() *DataChannel {
	return b.InnerInDataChannel[0]
}

func (b *Block) GetInnerOutData() *DataChannel {
	return b.InnerOutDataChannel[0]
}
