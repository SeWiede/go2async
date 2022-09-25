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

func (sv *ScopedVariables) NewVariableFromInfo(vi *VariableInfo) (*VariableInfo, error) {
	return sv.NewVariable(&VariableTypeDecl{
		Name:     vi.Name,
		Typ:      vi.Typ,
		Len:      vi.Len,
		FuncIntf: vi.FuncIntf,
	})
}

func (sv *ScopedVariables) NewVariable(decl *VariableTypeDecl) (*VariableInfo, error) {
	if decl == nil {
		return nil, ErrNilDecl
	}

	typeSize := -1

	funcIntf := decl.FuncIntf

	if decl.FuncIntf == nil {
		if decl.Len <= 0 {
			return nil, ErrInvalidVariableLength
		}

		var ok bool
		typeSize, ok = SupportedTypes[decl.Typ]
		if !ok {
			return nil, ErrUnsupportedVariableTypeFn(decl.Typ)
		}
	} else {
		// typeSize if result size
		typeSize = decl.FuncIntf.Results.GetSize()
		funcIntf = funcIntf.Copy()
	}

	if decl.Name == "" {
		decl.Name = emptyNamePrefix + strconv.Itoa(sv.ParamPos)
		sv.ParamPos++
	}

	newV := &VariableInfo{
		Name:     decl.Name,
		Position: sv.Size,
		Size:     typeSize,
		Typ:      decl.Typ,
		Len:      decl.Len,
		FuncIntf: funcIntf,
	}

	if _, ok := sv.Variables[decl.Name]; ok {
		return nil, ErrVariableAlreadyDeclaredFn(decl.Name)
	}

	sv.Variables[decl.Name] = newV

	infoprinter.VerbosePrintf("Allocated %s at pos %d downto %d\n", decl.Name, (sv.Size)+(decl.Len*typeSize)-1, (sv.Size))

	sv.Size += typeSize * decl.Len

	sv.VariableList = append(sv.VariableList, newV)

	return newV.Copy(), nil
}

func (sv *ScopedVariables) GetSize() int {
	return sv.Size
}
