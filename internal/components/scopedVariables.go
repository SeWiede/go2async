package components

import (
	"errors"
	"fmt"
	"go2async/pkg/variable"
)

var ErrVariableNotFound = errors.New("Variable not found")
var ErrUnsupportedVariableType = errors.New("Unsupported variable type")
var ErrInvalidVariableLength = errors.New("Invalid variable length - has to be greater than 0")

type ScopedVariables struct {
	variables map[string]*variable.VariableInfo
	size      int
}

func NewScopedVarialbes(parent *Block) *ScopedVariables {
	parentSize := 0
	if parent != nil {
		parentSize = parent.scopedVariables.size
	}

	return &ScopedVariables{
		variables: make(map[string]*variable.VariableInfo),
		size:      parentSize,
	}
}

func (sv *ScopedVariables) GetVariableInfo(name string) (*variable.VariableInfo, error) {
	vi, ok := sv.variables[name]
	if !ok {
		return nil, ErrVariableNotFound
	}

	return vi.Copy(), nil
}

func (sv *ScopedVariables) AddVariable(name string, typ string, len int) (*variable.VariableInfo, error) {
	typeSize, ok := SupportedTypes[typ]
	if !ok {
		fmt.Errorf("Encountered unsupported variable type: '%s'", typ)
		return nil, ErrUnsupportedVariableType
	}

	if len <= 0 {
		return nil, ErrInvalidVariableLength
	}

	newV := &variable.VariableInfo{
		Position: sv.size,
		Size:     typeSize,
		Typ:      typ,
		Len:      len,
	}

	sv.variables[name] = newV

	fmt.Printf("Allocated %s at pos %d downto %d\n", name, (sv.size)+(len*typeSize)-1, (sv.size))

	sv.size += typeSize * len

	return newV.Copy(), nil
}

func (sv *ScopedVariables) GetSize() int {
	return sv.size
}
