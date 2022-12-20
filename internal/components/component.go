package components

import (
	"errors"
	"go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"strconv"
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
	Parent() *Block
}

type BodyComponentType interface {
	Component
	InChannel() *HandshakeChannel
	OutChannel() *HandshakeChannel

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
}

type BodyComponent struct {
	parentBlock *Block

	number int

	archName string

	In  *HandshakeChannel
	Out *HandshakeChannel

	predecessors map[string]BodyComponentType
	successors   map[string]BodyComponentType

	inputVariables  *variable.ScopedVariables
	outputVariables *variable.ScopedVariables

	isBlock bool
}

// Type checks
func (bc *BodyComponent) Name() string {
	return strconv.Itoa(bc.number) + "__RESERVED__"
}

func (bc *BodyComponent) ArchName() string {
	return bc.archName
}

func (bc *BodyComponent) Parent() *Block {
	return bc.parentBlock
}

func (bc *BodyComponent) InChannel() *HandshakeChannel {
	return bc.In
}

func (bc *BodyComponent) OutChannel() *HandshakeChannel {
	return bc.Out
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

	if !bc.isBlock {
		// Check owner map
		parent := bc.parentBlock
		if _, ok := parent.VariableOwner[vi.Name()]; !ok {

			parent.VariableOwner[vi.Name()] = &variableOwner{
				ownerList: NewOwnerList(parent),
				vi:        vtd,
			}

			infoPrinter.DebugPrintfln("[%s]: No owner for variable '%s' found. Making parent %s owner", bc.Name(), vi.Name(), parent.Name())
		}
	}

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
	return bc.OutputVariables().AddVariable(vtd)
}

func (bc *BodyComponent) GetVariableLocation(name string) (string, error) {
	return "", errors.New("not implemented")
}
