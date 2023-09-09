package variable

import (
	"errors"
	infoPrinter "go2async/internal/infoPrinter"
	"strconv"
)

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

func (sv *ScopedVariables) GetActualVariableInfo(name string) (*VariableInfo, error) {
	vi, ok := sv.Variables[name]
	if !ok {
		return nil, ErrVariableNotFoundFn(name)
	}

	return vi, nil
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
	signed := false

	funcIntf := v.FuncIntf()

	if v.FuncIntf() == nil {
		if v.Len() <= 0 {
			return nil, ErrInvalidVariableLength
		}

		var ok bool
		typeInfo, ok := SupportedTypes[v.Typ()]
		if !ok {
			return nil, ErrUnsupportedVariableTypeFn(v.Typ())
		}

		typeSize = typeInfo.Size
		signed = typeInfo.Signed
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
		Name_:     name,
		Position_: sv.Size,
		Size_:     typeSize,
		Signed_:   signed,
		Typ_:      v.Typ(),
		Len_:      v.Len(),
		Const_:    v.Const(),

		// TODO: add index!

		FuncIntf_: funcIntf,
	}

	if _, ok := sv.Variables[name]; ok {
		return nil, ErrVariableAlreadyDeclaredFn(name)
	}

	sv.Variables[name] = newV

	infoPrinter.VerbosePrintf("Allocated '%s' type %s at pos %d downto %d\n", newV.Name(), v.Typ(), (sv.Size)+(v.Len()*typeSize)-1, (sv.Size))

	sv.Size += typeSize * v.Len()

	sv.VariableList = append(sv.VariableList, newV)

	return newV.Copy(), nil
}

func (sv *ScopedVariables) AddVariableInfo(v *VariableInfo) (*VariableInfo, error) {
	if v == nil {
		return nil, ErrNilDecl
	}

	typeSize := -1
	signed := false

	funcIntf := v.FuncIntf()

	if v.FuncIntf() == nil {
		if v.Len() <= 0 {
			return nil, ErrInvalidVariableLength
		}

		var ok bool
		typeInfo, ok := SupportedTypes[v.Typ()]
		if !ok {
			return nil, ErrUnsupportedVariableTypeFn(v.Typ())
		}

		typeSize = typeInfo.Size
		signed = typeInfo.Signed
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
		Name_:     name,
		Position_: sv.Size,
		Size_:     typeSize,
		Signed_:   signed,
		Typ_:      v.Typ(),
		Len_:      v.Len(),
		Const_:    v.Const(),

		FuncIntf_: funcIntf,
	}

	if _, ok := sv.Variables[name]; ok {
		return nil, ErrVariableAlreadyDeclaredFn(name)
	}

	sv.Variables[name] = newV

	infoPrinter.VerbosePrintf("Allocated '%s' type %s at pos %d downto %d with index %s\n", newV.Name(), v.Typ(), (sv.Size)+(v.Len()*typeSize)-1, (sv.Size), newV.Index_)

	sv.Size += typeSize * v.Len()

	sv.VariableList = append(sv.VariableList, newV)

	return newV.Copy(), nil
}

func (sv *ScopedVariables) GetSize() int {
	return sv.Size
}

func (sv *ScopedVariables) HasVariableName(v string) bool {
	_, ok := sv.Variables[v]
	return ok
}

func (sv *ScopedVariables) HasVariable(vi *VariableInfo) bool {
	return sv.HasVariableName(vi.Name())
}
