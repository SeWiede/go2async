package components

import (
	"errors"
	infoprinter "go2async/internal/infoPrinter"
	"go2async/pkg/variable"
)

var ErrInvalidVariableLength = errors.New("Invalid variable length - has to be greater than 0")

func ErrVariableAlreadyDeclaredFn(name string) error {
	return errors.New("Variable '" + name + "' already declared in current scope")
}
func ErrVariableNotFoundFn(name string) error {
	return errors.New("Variable '" + name + "' not found")
}
func ErrUnsupportedVariableTypeFn(typ string) error {
	return errors.New("Unsupported variable type " + typ)
}

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
		return nil, ErrVariableNotFoundFn(name)
	}

	return vi.Copy(), nil
}

func (sv *ScopedVariables) AddVariable(name string, typ string, len int) (*variable.VariableInfo, error) {
	typeSize, ok := SupportedTypes[typ]
	if !ok {
		return nil, ErrUnsupportedVariableTypeFn(typ)
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

	if _, ok := sv.variables[name]; ok {
		return nil, ErrVariableAlreadyDeclaredFn(name)
	}

	sv.variables[name] = newV

	infoprinter.VerbosePrintf("Allocated %s at pos %d downto %d\n", name, (sv.size)+(len*typeSize)-1, (sv.size))

	sv.size += typeSize * len

	return newV.Copy(), nil
}

func (sv *ScopedVariables) GetSize() int {
	return sv.size
}
