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
	GetAndAssignFunctionInterface(fname string) (*variable.VariableInfo, error)
	GetVariableOwnerMap() map[string]*variableOwner
	GetExternalInterfaces() map[string]*variable.VariableInfo
	AddComponent(bodyComponent BodyComponentType)
	NewScopeVariable(vdef variable.VariableDef) (*variable.VariableInfo, error)
}

func (b *Block) GetExternalInterfaces() map[string]*variable.VariableInfo {
	return b.ExternalInterfaces
}

func (b *Block) GetVariableOwnerMap() map[string]*variableOwner {
	return b.VariableOwner
}

func (b *IfBlock) GetExternalInterfaces() map[string]*variable.VariableInfo {
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

	vi, err := bc.InputVariables().AddVariable(vtd)
	if err != nil {
		return nil, err
	}

	if !bc.isBlock {
		// Check owner map
		parent := bc.parentBlock
		if _, ok := parent.GetVariableOwnerMap()[vi.Name()]; !ok {

			parent.GetVariableOwnerMap()[vi.Name()] = &variableOwner{
				ownerList: NewOwnerList(parent),
				vi:        vtd,
			}

			infoPrinter.DebugPrintfln("[%s]: No owner for variable '%s' found. Making parent %s owner", bc.archName, vi.Name(), parent.Name())
		}
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

	return bc.OutputVariables().AddVariable(vtd)
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
