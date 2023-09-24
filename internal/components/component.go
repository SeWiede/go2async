package components

import (
	"errors"
	"go2async/internal/infoPrinter"
	"go2async/internal/variable"
)

const archPrefix = "beh_"
const defaultArch = "behavioural"

var zero = 0
var one = 1

type Component interface {
	Name() string
	ArchName() string
	ComponentStr() string
	Architecture() string
	Entity() string
	EntityName() string
	Parent() BlockType
}

type variableOwner struct {
	// Linked list of predecessors
	ownerList *ownerList
	vi        *variable.VariableInfo
}

type BlockType interface {
	BodyComponentType

	GetVariable(name string) (*variable.VariableInfo, error)
	GetAndAssignFunctionInterface(fname string) (*ExternalInterface, error)
	GetVariableOwnerMap() map[string]*variableOwner
	GetExternalInterfaces() map[string]*ExternalInterface
	AddComponent(bodyComponent BodyComponentType)
	NewScopeVariable(vdef variable.VariableDef) (*variable.VariableInfo, error)

	GetInnerInData() *DataChannel
	GetInnerOutData() *DataChannel
	GetInnerIn() *HandshakeChannel
	GetInnerOut() *HandshakeChannel
}

func (b *Block) GetExternalInterfaces() map[string]*ExternalInterface {
	return b.ExternalInterfaces
}

func (b *Block) GetVariableOwnerMap() map[string]*variableOwner {
	return b.VariableOwner
}

func (b *IfBlock) GetExternalInterfaces() map[string]*ExternalInterface {
	return b.ExternalInterfaces
}

func (b *IfBlock) GetVariableOwnerMap() map[string]*variableOwner {
	return b.VariableOwner
}

type BodyComponentType interface {
	Component
	InChannels() []*HandshakeChannel
	OutChannels() []*HandshakeChannel

	InDataChannels() []*DataChannel
	OutDataChannels() []*DataChannel

	GetVariableSize() int

	Predecessors() map[string]BodyComponentType
	AddPredecessor(BodyComponentType)
	Successors() map[string]BodyComponentType
	AddSuccessor(BodyComponentType)

	InputVariables() *variable.ScopedVariables
	AddInputVariable(*variable.VariableInfo) (*variable.VariableInfo, error)
	OutputVariables() *variable.ScopedVariables
	AddOutputVariable(*variable.VariableInfo) (*variable.VariableInfo, error)

	GetVariableLocation(string) (string, error)

	GetSignalDefs() string

	ConnectHandshake(bct BodyComponentType)
	ConnectData(bct BodyComponentType)

	ConnectHandshakeDir(bct BodyComponentType, out bool)
	ConnectDataDir(bct BodyComponentType, out bool)

	ConnectHandshakePos(bct BodyComponentType, from, to int)
	ConnectDataPos(bct BodyComponentType, from, to int)

	ConnectHandshakePosDir(bct BodyComponentType, from, to int, out bool)
	ConnectDataPosDir(bct BodyComponentType, from, to int, out bool)

	GetHandshakeSignalAssigmentStr() string
	GetDataSignalAssigmentStr() string

	ConnectVariable(bct BodyComponentType, vi *variable.VariableInfo)
	ConnectOutVariable(bct BodyComponentType, vi *variable.VariableInfo)
}

type BodyComponent struct {
	name        string
	parentBlock BlockType

	number int

	archName string

	In  []*HandshakeChannel
	Out []*HandshakeChannel

	InData  []*DataChannel
	OutData []*DataChannel

	predecessors map[string]BodyComponentType
	successors   map[string]BodyComponentType

	inputVariables  *variable.ScopedVariables
	outputVariables *variable.ScopedVariables

	isBlock bool
}

// Type checks
func (bc *BodyComponent) Name() string {
	return bc.name
}

func (bc *BodyComponent) ArchName() string {
	return bc.archName
}

func (bc *BodyComponent) Parent() BlockType {
	return bc.parentBlock
}

func (bc *BodyComponent) InChannels() []*HandshakeChannel {
	return bc.In
}

func (bc *BodyComponent) OutChannels() []*HandshakeChannel {
	return bc.Out
}

func (bc *BodyComponent) InDataChannels() []*DataChannel {
	return bc.InData
}

func (bc *BodyComponent) OutDataChannels() []*DataChannel {
	return bc.OutData
}

func (bc *BodyComponent) GetVariableSize() int {
	return bc.InputVariables().Size
}

func (bc *BodyComponent) Predecessors() map[string]BodyComponentType {
	return bc.predecessors
}

func (bc *BodyComponent) AddPredecessor(bct BodyComponentType) {
	bc.predecessors[bct.Name()] = bct
}

func (bc *BodyComponent) Successors() map[string]BodyComponentType {
	return bc.successors
}

func (bc *BodyComponent) AddSuccessor(bct BodyComponentType) {
	bc.successors[bct.Name()] = bct
}

func (bc *BodyComponent) InputVariables() *variable.ScopedVariables {
	return bc.inputVariables
}

func (bc *BodyComponent) AddInputVariable(vtd *variable.VariableInfo) (*variable.VariableInfo, error) {

	vi, err := bc.InputVariables().AddVariableInfo(vtd)
	if err != nil {
		return nil, err
	}

	// Check owner map
	parent := bc.parentBlock
	if _, ok := parent.GetVariableOwnerMap()[vi.Name()]; !ok {

		parent.GetVariableOwnerMap()[vi.Name()] = &variableOwner{
			ownerList: NewOwnerList(parent),
			vi:        vtd,
		}

		infoPrinter.DebugPrintfln("[%s]: No owner for variable '%s' found. Making parent %s owner", bc.archName, vi.Name(), parent.Name())
	}

	/* 	if len(bc.InData) > 0 {
	   		bc.InData[0].AddVariable(vi)
	   	} else {
	   		infoPrinter.DebugPrintfln("[%s]: has no inData", bc.Name())
	   	} */

	return vi, err
}

func (bc *BodyComponent) OutputVariables() *variable.ScopedVariables {
	if bc.outputVariables == nil {
		// Default is output = input
		return bc.inputVariables
	}

	return bc.outputVariables
}

func (bc *BodyComponent) AddOutputVariable(vtd *variable.VariableInfo) (*variable.VariableInfo, error) {
	/* if len(bc.OutData) > 0 {
		bc.OutData[0].AddVariable(vtd)
	} else {
		infoPrinter.DebugPrintfln("[%s]: has no outData", bc.Name())
	} */

	return bc.OutputVariables().AddVariableInfo(vtd)
}

func (bc *BodyComponent) GetVariableLocation(name string) (string, error) {
	return "", errors.New("not implemented")
}

func (bc *BodyComponent) GetSignalDefs() string {
	signalDefs := ""

	for _, in := range bc.In {
		signalDefs += in.SignalDefs()
	}
	for _, out := range bc.Out {
		signalDefs += out.SignalDefs()
	}

	for _, in := range bc.InData {
		signalDefs += in.SignalDefs()
	}
	for _, out := range bc.OutData {
		signalDefs += out.SignalDefs()
	}

	return signalDefs
}

func (bc *BodyComponent) ConnectHandshake(bct BodyComponentType) {
	bc.ConnectHandshakePos(bct, 0, 0)
}

func (bc *BodyComponent) ConnectData(bct BodyComponentType) {
	bc.ConnectDataPos(bct, 0, 0)
}

func (bc *BodyComponent) ConnectHandshakeDir(bct BodyComponentType, out bool) {
	bc.ConnectHandshakePosDir(bct, 0, 0, out)
}

func (bc *BodyComponent) ConnectDataDir(bct BodyComponentType, out bool) {
	bc.ConnectDataPosDir(bct, 0, 0, out)
}

func (bc *BodyComponent) ConnectHandshakePos(bct BodyComponentType, from, to int) {
	bc.ConnectHandshakePosDir(bct, from, to, false)
}

func (bc *BodyComponent) ConnectDataPos(bct BodyComponentType, from, to int) {
	bc.ConnectDataPosDir(bct, from, to, false)
}

func (bc *BodyComponent) ConnectHandshakePosDir(bct BodyComponentType, from, to int, out bool) {
	infoPrinter.DebugPrintfln("[%s]: connecting handshakes to '%s' (out? %t from pos %d to pos %d)", bc.Name(), bct.Name(), out, from, to)

	var fromHsChannel *HandshakeChannel
	var toHsChannel *HandshakeChannel

	if out {
		fromHsChannel = bc.OutChannels()[from]
		toHsChannel = bct.InChannels()[to]
	} else {
		fromHsChannel = bc.InChannels()[from]
		toHsChannel = bct.OutChannels()[to]
	}

	if bc.parentBlock == nil {
		panic("parent of " + bc.Name() + " is nil")
	}

	if bct == bc.parentBlock || bct.Name() == bc.parentBlock.Name() {
		if bct != bc.parentBlock {
			infoPrinter.DebugPrintfln("[%s]: bct %s is parent but not as pointer! %p != %p", bc.Name(), bct.Name(), bc.parentBlock, bct)
		}

		infoPrinter.DebugPrintfln("[%s]: to handshake connection is parent '%s' - using parent's inner connections", bc.name, bc.Parent().Name())

		if out {
			toHsChannel = bc.parentBlock.GetInnerOut()
		} else {
			toHsChannel = bc.parentBlock.GetInnerIn()
		}
	}

	fromHsChannel.ConnectHandshake(toHsChannel)

	infoPrinter.DebugPrintfln("[%s]: connected %d. handshake to '%s' %d.", bc.Name(), from, bct.Name(), to)
}

func (bc *BodyComponent) ConnectDataPosDir(bct BodyComponentType, from, to int, out bool) {
	infoPrinter.DebugPrintfln("[%s]: connecting data to %s (out? %t from pos %d to pos %d)", bc.Name(), bct.Name(), out, from, to)
	var fromDataChannel *DataChannel
	var toDataChannel *DataChannel

	if out {
		fromDataChannel = bc.OutDataChannels()[from]
		toDataChannel = bct.InDataChannels()[to]
	} else {
		fromDataChannel = bc.InDataChannels()[from]
		toDataChannel = bct.OutDataChannels()[to]
	}

	if bct == bc.parentBlock || bct.Name() == bc.parentBlock.Name() {
		infoPrinter.DebugPrintfln("[%s]: to data connection is parent '%s' - using parent's inner connections", bc.name, bc.Parent().Name())

		if out {
			toDataChannel = bc.parentBlock.GetInnerOutData()
		} else {
			toDataChannel = bc.parentBlock.GetInnerInData()
		}
	}

	if err := fromDataChannel.ConnectData(toDataChannel); err != nil {
		// panic("could not connect data: " + err.Error())
		infoPrinter.DebugPrintfln("[%s]: error connecting data to %s: %s", bc.Name(), bct.Name(), err.Error())
	}

	infoPrinter.DebugPrintfln("[%s]: %s connected %d. data to '%s' %s %d.", bc.Name(), fromDataChannel.DataName, from, bct.Name(), toDataChannel.DataName, to)
}

func (bc *BodyComponent) ConnectVariable(bct BodyComponentType, vi *variable.VariableInfo) {
	infoPrinter.DebugPrintfln("[%s]: connecting variable to %s (out? %t from pos %d to pos %d)", bc.Name(), bct.Name(), false, 0, 0)
	var fromDataChannel *DataChannel
	var toDataChannel *DataChannel

	fromDataChannel = bc.InDataChannels()[0]
	toDataChannel = bct.OutDataChannels()[0]

	if bct == bc.parentBlock || bct.Name() == bc.parentBlock.Name() {
		infoPrinter.DebugPrintfln("[%s]: to data connection is parent '%s' - using parent's inner connections", bc.name, bc.Parent().Name())

		toDataChannel = bc.parentBlock.GetInnerInData()
	}

	if err := fromDataChannel.ConnectVariable(toDataChannel, vi); err != nil {
		panic("could not connect data: " + err.Error())
	}

	infoPrinter.DebugPrintfln("[%s]: %s connected %d. variable to '%s' %s %d.", bc.Name(), fromDataChannel.DataName, 0, bct.Name(), toDataChannel.DataName, 0)
}
func (bc *BodyComponent) ConnectOutVariable(bct BodyComponentType, vi *variable.VariableInfo) {
	infoPrinter.DebugPrintfln("[%s]: connecting variable to %s (out? %t from pos %d to pos %d)", bc.Name(), bct.Name(), true, 0, 0)
	var fromDataChannel *DataChannel
	var toDataChannel *DataChannel

	fromDataChannel = bc.OutDataChannels()[0]
	toDataChannel = bct.InDataChannels()[0]

	if bct == bc.parentBlock || bct.Name() == bc.parentBlock.Name() {
		infoPrinter.DebugPrintfln("[%s]: to data connection is parent '%s' - using parent's inner connections", bc.name, bc.Parent().Name())

		toDataChannel = bc.parentBlock.GetInnerOutData()
	}

	if err := fromDataChannel.ConnectVariable(toDataChannel, vi); err != nil {
		panic("could not connect data: " + err.Error())
	}

	infoPrinter.DebugPrintfln("[%s]: %s connected %d. variable to '%s' %s %d.", bc.Name(), fromDataChannel.DataName, 0, bct.Name(), toDataChannel.DataName, 0)
}

func (bc *BodyComponent) GetHandshakeSignalAssigmentStr() string {
	handShakeAssignments := ""

	for _, in := range bc.InChannels() {
		handShakeAssignments += in.GetSignalAssigmentStr()

		handShakeAssignments += "\n"
	}

	handShakeAssignments += "\n"

	for _, out := range bc.OutChannels() {
		handShakeAssignments += out.GetSignalAssigmentStr()

		handShakeAssignments += "\n"
	}

	return handShakeAssignments
}

func (bc *BodyComponent) GetDataSignalAssigmentStr() string {
	dataSignalAssignments := ""

	for _, in := range bc.InDataChannels() {
		dataSignalAssignments += in.GetSignalAssigmentStr()
		dataSignalAssignments += "\n"
	}

	for _, out := range bc.OutDataChannels() {
		dataSignalAssignments += out.GetSignalAssigmentStr()
		dataSignalAssignments += "\n"
	}

	return dataSignalAssignments
}
