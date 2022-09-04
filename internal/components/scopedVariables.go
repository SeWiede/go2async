package components

import (
	"errors"
	infoprinter "go2async/internal/infoPrinter"
	"go2async/pkg/variable"
	"strconv"
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
func ErrInvalidVariablePosFn(pos int) error {
	return errors.New("Variableposition " + strconv.Itoa(pos) + " is invalid")
}

var emptyNamePrefix = "__param_"

type ScopedVariables struct {
	variables    map[string]*variable.VariableInfo
	variableList []*variable.VariableInfo
	size         int
	paramPos     int
}

func NewScopedVariables(parent *Block) *ScopedVariables {
	parentSize := 0
	if parent != nil {
		parentSize = parent.scopedVariables.size
	}

	return &ScopedVariables{
		variables:    make(map[string]*variable.VariableInfo),
		variableList: []*variable.VariableInfo{},
		size:         parentSize,
		paramPos:     0,
	}
}

func (sv *ScopedVariables) GetVariableInfo(name string) (*variable.VariableInfo, error) {
	vi, ok := sv.variables[name]
	if !ok {
		return nil, ErrVariableNotFoundFn(name)
	}

	return vi.Copy(), nil
}

func (sv *ScopedVariables) GetVariableInfoAt(pos int) (*variable.VariableInfo, error) {
	if len(sv.variableList) <= pos || pos < 0 {
		return nil, ErrInvalidVariablePosFn(pos)
	}

	return sv.variableList[pos].Copy(), nil
}

func (sv *ScopedVariables) AddVariable(name string, typ string, len int) (*variable.VariableInfo, error) {
	typeSize, ok := SupportedTypes[typ]
	if !ok {
		return nil, ErrUnsupportedVariableTypeFn(typ)
	}

	if len <= 0 {
		return nil, ErrInvalidVariableLength
	}

	if name == "" {
		name = emptyNamePrefix + strconv.Itoa(sv.paramPos)
		sv.paramPos++
	}

	newV := &variable.VariableInfo{
		Name:     name,
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

	sv.variableList = append(sv.variableList, newV)

	return newV.Copy(), nil
}

func (sv *ScopedVariables) GetSize() int {
	return sv.size
}
