package variable

import (
	"errors"
	infoprinter "go2async/internal/infoPrinter"
	"strconv"
)

var SupportedTypes map[string]int = map[string]int{"int": strconv.IntSize, "int8": 8, "int16": 16, "int32": 32, "int64": 64, "uint": strconv.IntSize, "uint8": 8, "uint16": 16, "uint32": 32, "uint64": 64, "byte": 8}

var ErrInvalidVariableLength = errors.New("Invalid variable length - has to be greater than 0")
var ErrNilDecl = errors.New("Variable decl is nil")

func ErrVariableAlreadyDeclaredFn(name string) error {
	return errors.New("Variable '" + name + "' already declared in current scope")
}
func ErrVariableNotFoundFn(name string) error {
	return errors.New("Variable '" + name + "' not found")
}
func ErrUnsupportedVariableTypeFn(typ string) error {
	return errors.New("Unsupported variable type '" + typ + "'")
}
func ErrInvalidVariablePosFn(pos int) error {
	return errors.New("Variableposition " + strconv.Itoa(pos) + " is invalid")
}

var emptyNamePrefix = "__param_"

type ScopedVariables struct {
	Variables    map[string]*VariableInfo
	VariableList []*VariableInfo
	Size         int
	ParamPos     int
}

type FunctionPrototype struct {
	ParamVarList         []*VariableInfo
	ResultVarList        []*VariableInfo
	HasExtrenalInterface bool
}

func NewScopedVariables() *ScopedVariables {
	return &ScopedVariables{
		Variables:    make(map[string]*VariableInfo),
		VariableList: []*VariableInfo{},
		Size:         0,
		ParamPos:     0,
	}
}

func (sv *ScopedVariables) Copy() *ScopedVariables {
	ret := NewScopedVariables()

	for k, v := range sv.Variables {
		ret.Variables[k] = v.Copy()
	}

	for _, v := range sv.VariableList {
		ret.VariableList = append(ret.VariableList, v.Copy())
	}

	ret.Size = sv.Size
	ret.ParamPos = sv.ParamPos

	return ret
}

func (sv *ScopedVariables) GetVariableInfo(name string) (*VariableInfo, error) {
	vi, ok := sv.Variables[name]
	if !ok {
		return nil, ErrVariableNotFoundFn(name)
	}

	return vi.Copy(), nil
}

func (sv *ScopedVariables) GetVariableInfoAt(pos int) (*VariableInfo, error) {
	if len(sv.VariableList) <= pos || pos < 0 {
		return nil, ErrInvalidVariablePosFn(pos)
	}

	return sv.VariableList[pos].Copy(), nil
}

func (sv *ScopedVariables) AddVariable(v VariableDef) (*VariableInfo, error) {
	if v == nil {
		return nil, ErrNilDecl
	}

	typeSize := -1

	funcIntf := v.FuncIntf()

	if v.FuncIntf() == nil {
		if v.Len() <= 0 {
			return nil, ErrInvalidVariableLength
		}

		var ok bool
		typeSize, ok = SupportedTypes[v.Typ()]
		if !ok {
			return nil, ErrUnsupportedVariableTypeFn(v.Typ())
		}
	} else {
		// typeSize if result size
		typeSize = v.FuncIntf().Results.GetSize()
		funcIntf = funcIntf.Copy()
	}

	name := v.Name()

	if name == "" {
		name = emptyNamePrefix + strconv.Itoa(sv.ParamPos)
		sv.ParamPos++
	}

	newV := &VariableInfo{
		Name_:     v.Name(),
		Position_: sv.Size,
		Size_:     typeSize,
		Typ_:      v.Typ(),
		Len_:      v.Len(),
		FuncIntf_: funcIntf,
	}

	if _, ok := sv.Variables[v.Name()]; ok {
		return nil, ErrVariableAlreadyDeclaredFn(v.Name())
	}

	sv.Variables[v.Name()] = newV

	infoprinter.VerbosePrintf("Allocated %s at pos %d downto %d\n", v.Name(), (sv.Size)+(v.Len()*typeSize)-1, (sv.Size))

	sv.Size += typeSize * v.Len()

	sv.VariableList = append(sv.VariableList, newV)

	return newV.Copy(), nil
}

func (sv *ScopedVariables) GetSize() int {
	return sv.Size
}