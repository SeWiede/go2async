package variable

import "strconv"

type FuncInterface struct {
	Parameters *ScopedVariables
	Results    *ScopedVariables
}

func NewFuncIntf() *FuncInterface {
	return &FuncInterface{
		Parameters: NewScopedVariables(),
		Results:    NewScopedVariables(),
	}
}

func (fi *FuncInterface) Copy() *FuncInterface {
	return &FuncInterface{
		Parameters: fi.Parameters.Copy(),
		Results:    fi.Results.Copy(),
	}
}

type VariableTypeDecl struct {
	Name string
	Typ  string
	Len  int

	FuncIntf *FuncInterface
}

func (vTd *VariableTypeDecl) Copy() *VariableTypeDecl {
	var copiedFuncIntf *FuncInterface
	if vTd.FuncIntf != nil {
		copiedFuncIntf = vTd.FuncIntf.Copy()
	}

	return &VariableTypeDecl{
		Name: vTd.Name,
		Typ:  vTd.Typ,
		Len:  vTd.Len,

		FuncIntf: copiedFuncIntf,
	}
}

type VariableInfo struct {
	Name       string
	Position   int
	Size       int
	Typ        string
	Const      string
	Len        int
	Index      string
	IndexIdent *VariableInfo

	FuncIntf *FuncInterface
}

func (vi *VariableInfo) Copy() *VariableInfo {
	var copiedIndexIdent *VariableInfo
	if vi.IndexIdent != nil {
		copiedIndexIdent = vi.IndexIdent.Copy()
	}

	var copiedFuncIntf *FuncInterface
	if vi.FuncIntf != nil {
		copiedFuncIntf = vi.FuncIntf.Copy()
	}

	return &VariableInfo{
		Name:       vi.Name,
		Position:   vi.Position,
		Size:       vi.Size,
		Typ:        vi.Typ,
		Const:      vi.Const,
		Len:        vi.Len,
		Index:      vi.Index,
		IndexIdent: copiedIndexIdent,

		FuncIntf: copiedFuncIntf,
	}
}

func (vi *VariableInfo) String() string {
	k := vi.Name
	lB := strconv.Itoa(vi.Position)
	uB := strconv.Itoa(vi.Position + vi.Size)
	if vi.Len > 1 {
		k = k + "[k]"
		lB += " + " + strconv.Itoa(vi.Size) + " * k"
		uB = strconv.Itoa(vi.Position) + " + " + strconv.Itoa(vi.Size) + " * (k + 1)"
	}

	return k + " is at (" + uB + " - 1 downto " + lB + ") "
}
