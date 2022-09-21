package components

import (
	"strconv"
)

const archPrefix = "beh_"
const defaultArch = "behavioural"

var SupportedTypes map[string]int = map[string]int{"int": strconv.IntSize, "int8": 8, "int16": 16, "int32": 32, "int64": 64, "uint": strconv.IntSize, "uint8": 8, "uint16": 16, "uint32": 32, "uint64": 64, "byte": 8}

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

func (bc *BodyComponent) ScopedVariables() *ScopedVariables {
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
