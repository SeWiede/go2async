package components

import (
	"errors"
	"fmt"
	"go2async/pkg/variable"
	"strconv"
)

const archPrefix = "beh_"
const defaultArch = "behavioural"

var SupportedTypes map[string]int = map[string]int{"int": strconv.IntSize, "int8": 8, "int16": 16, "int32": 32, "int64": 64, "uint": strconv.IntSize, "uint8": 8, "uint16": 16, "uint32": 32, "uint64": 64, "byte": 8}

var zero = 0
var one = 1

type Component interface {
	Component() string
	Architecture() string
	ArchName() string
}

type BodyComponent interface {
	Component
	InChannel() *HandshakeChannel
	OutChannel() *HandshakeChannel

	ScopedVariables() map[string]*variable.VariableInfo
	Predecessor() BodyComponent
	GetVariablesSize() *int
}

func NewVariable(receiver BodyComponent, name string, typ string, len int) (*variable.VariableInfo, error) {
	if len <= 0 {
		return nil, errors.New("Invalid variable len")
	}

	size, ok := SupportedTypes[typ]
	if !ok {
		return nil, errors.New("Unsupported type '" + typ + "'")
	}

	cvp := receiver.GetVariablesSize()

	newV := &variable.VariableInfo{
		Position: *cvp,
		Size:     size,
		Typ:      typ,
		Len:      len,
	}

	receiver.ScopedVariables()[name] = newV

	fmt.Printf("Allocated %s at pos %d downto %d\n", name, (*cvp)+(len*size)-1, (*cvp))

	*cvp += size * len

	return newV.Copy(), nil
}

func GetVariable(receiver BodyComponent, name string) *variable.VariableInfo {
	if receiver == nil {
		return nil
	}

	v, ok := receiver.ScopedVariables()[name]
	if !ok {
		v = GetVariable(receiver.Predecessor(), name)
	} else {
		fmt.Printf("Got variable %s at %d downto %d\n", name, (v.Position + v.Len*v.Size - 1), v.Position)
	}

	return v.Copy()
}
