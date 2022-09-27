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

type VariableDef interface {
	Name() string
	Typ() string
	Len() int

	FuncIntf() *FuncInterface
}

type VariableTypeDecl struct {
	Name_ string
	Typ_  string
	Len_  int

	FuncIntf_ *FuncInterface
}

func (vTd *VariableTypeDecl) Name() string {
	return vTd.Name_
}

func (vTd *VariableTypeDecl) Typ() string {
	return vTd.Typ_
}

func (vTd *VariableTypeDecl) Len() int {
	return vTd.Len_
}

func (vTd *VariableTypeDecl) FuncIntf() *FuncInterface {
	return vTd.FuncIntf_
}

func (vTd *VariableTypeDecl) Copy() *VariableTypeDecl {
	var copiedFuncIntf *FuncInterface
	if vTd.FuncIntf_ != nil {
		copiedFuncIntf = vTd.FuncIntf_.Copy()
	}

	return &VariableTypeDecl{
		Name_: vTd.Name_,
		Typ_:  vTd.Typ_,
		Len_:  vTd.Len_,

		FuncIntf_: copiedFuncIntf,
	}
}

type VariableInfo struct {
	Name_       string
	Position_   int
	Size_       int
	Typ_        string
	Const_      string
	Len_        int
	Index_      string
	IndexIdent_ *VariableInfo

	FuncIntf_ *FuncInterface
}

func (vTd *VariableInfo) Name() string {
	return vTd.Name_
}

func (vTd *VariableInfo) Typ() string {
	return vTd.Typ_
}

func (vTd *VariableInfo) Len() int {
	return vTd.Len_
}

func (vTd *VariableInfo) FuncIntf() *FuncInterface {
	return vTd.FuncIntf_
}

func (vi *VariableInfo) Copy() *VariableInfo {
	var copiedIndexIdent *VariableInfo
	if vi.IndexIdent_ != nil {
		copiedIndexIdent = vi.IndexIdent_.Copy()
	}

	var copiedFuncIntf *FuncInterface
	if vi.FuncIntf_ != nil {
		copiedFuncIntf = vi.FuncIntf_.Copy()
	}

	return &VariableInfo{
		Name_:       vi.Name_,
		Position_:   vi.Position_,
		Size_:       vi.Size_,
		Typ_:        vi.Typ_,
		Const_:      vi.Const_,
		Len_:        vi.Len_,
		Index_:      vi.Index_,
		IndexIdent_: copiedIndexIdent,

		FuncIntf_: copiedFuncIntf,
	}
}

func (vi *VariableInfo) String() string {
	k := vi.Name_
	lB := strconv.Itoa(vi.Position_)
	uB := strconv.Itoa(vi.Position_ + vi.Size_)
	if vi.Len_ > 1 {
		k = k + "[k]"
		lB += " + " + strconv.Itoa(vi.Size_) + " * k"
		uB = strconv.Itoa(vi.Position_) + " + " + strconv.Itoa(vi.Size_) + " * (k + 1)"
	}

	return k + " is at (" + uB + " - 1 downto " + lB + ") "
}
