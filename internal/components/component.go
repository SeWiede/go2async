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

/*
type DefaultBlock struct {
	BodyComponent

	VariableOwner      map[string]*variableOwner
	ExternalInterfaces map[string]*variable.VariableInfo

	RegBlockPairs []*regBodyPair
}

func (b *DefaultBlock) NewScopeVariable(vdef variable.VariableDef) (*variable.VariableInfo, error) {
	infoPrinter.DebugPrintfln("[%s]: Adding variable %s ", b.ArchName(), vdef.Name())

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

func (b *DefaultBlock) GetVariable(name string) (*variable.VariableInfo, error) {
	infoPrinter.DebugPrintfln("[%s]: Getting variable %s", b.Name(), name)

	own, ok := b.VariableOwner[name]
	if ok {
		infoPrinter.DebugPrintfln("Variable %s's latest owner is %s", name, own.ownerList.lastest.Name())
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
		b.Parent().AddSuccessor(b)

		// Track variables that are coming from outside this block's scope.
		vi, err = b.NewScopeVariable(vi)
		if err != nil {
			return nil, err
		}

		vi.DefinedOnly_ = false

		// New owner of variable in current block is the current block.
		b.VariableOwner[vi.Name()] = &variableOwner{
			ownerList: NewOwnerList(b),
			vi:        vi,
		}

		b.InputVariables().AddVariable(vi)

		infoPrinter.DebugPrintfln("[%s]: Variable %s is from outside the block's scope. Added to inputs (current size = %d).", b.Name(), name, b.InputVariables().Size)

		return vi, nil
	}
}

func (b *DefaultBlock) GetExternalInterfaces() map[string]*variable.VariableInfo {
	return b.ExternalInterfaces
}

func (b *DefaultBlock) GetVariableOwnerMap() map[string]*variableOwner {
	return b.VariableOwner
}

func (b *DefaultBlock) AddComponent(bodyComponent BodyComponentType) {
	b.RegBlockPairs = append(b.RegBlockPairs, &regBodyPair{Bc: bodyComponent})
}

func (b *DefaultBlock) GetAndAssignFunctionInterface(fname string) (*variable.VariableInfo, error) {
	if f, ok := b.ExternalInterfaces[fname]; ok {
		return f, nil
	}

	infoPrinter.DebugPrintf("[%s] Function '%s' not found - searching parent\n", b.ArchName(), fname)

	if b.Parent() != nil {
		f, err := b.Parent().GetAndAssignFunctionInterface(fname)
		if err == nil {
			// parent-stack had the function defined: add the interface to block
			fiCopy := f.Copy()
			b.ExternalInterfaces[f.Name_] = fiCopy

			infoPrinter.DebugPrintf("[%s] Found function '%s' on parent stack and registerd function '%s'\n", b.ArchName(), f.Name_, f.Name_)

			return fiCopy, nil
		}
	}

	return nil, errors.New("No function '" + fname + "' found ")
}*/

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
	parentBlock BlockType

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
	panic("Encountered unnamed component " + bc.archName)
}

func (bc *BodyComponent) ArchName() string {
	return bc.archName
}

func (bc *BodyComponent) Parent() BlockType {
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
		if _, ok := parent.GetVariableOwnerMap()[vi.Name()]; !ok {

			parent.GetVariableOwnerMap()[vi.Name()] = &variableOwner{
				ownerList: NewOwnerList(parent),
				vi:        vtd,
			}

			infoPrinter.DebugPrintfln("[%s]: No owner for variable '%s' found. Making parent %s owner", bc.archName, vi.Name(), parent.Name())
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
