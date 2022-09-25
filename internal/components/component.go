package components

import "go2async/pkg/variable"

const archPrefix = "beh_"
const defaultArch = "behavioural"

var zero = 0
var one = 1

type Component interface {
	ArchName() string
	ComponentStr() string
	Architecture() string
	Entity() string
	EntityName() string
}

type BodyComponentType interface {
	Component
	InChannel() *HandshakeChannel
	OutChannel() *HandshakeChannel

	GetVariableSize() int
}

type BodyComponent struct {
	parent *Block

	archName string

	In  *HandshakeChannel
	Out *HandshakeChannel

	variableSize int
}

// Type checks
func (bc *BodyComponent) ArchName() string {
	return bc.archName
}

func (bc *BodyComponent) ScopedVariables() *variable.ScopedVariables {
	return bc.Parent().ScopedVariables()
}

func (bc *BodyComponent) Parent() *Block {
	return bc.parent
}

func (bc *BodyComponent) InChannel() *HandshakeChannel {
	return bc.In
}

func (bc *BodyComponent) OutChannel() *HandshakeChannel {
	return bc.Out
}

func (bc *BodyComponent) GetVariableSize() int {
	return bc.variableSize
}
