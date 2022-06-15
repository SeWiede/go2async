package variable

import "strconv"

type VariableInfo struct {
	Name       string
	Position   int
	Size       int
	Typ        string
	Const      string
	Len        int
	Index      string
	IndexIdent *VariableInfo
}

func (vi *VariableInfo) Copy() *VariableInfo {
	return &VariableInfo{
		Name:       vi.Name,
		Position:   vi.Position,
		Size:       vi.Size,
		Typ:        vi.Typ,
		Const:      vi.Const,
		Len:        vi.Len,
		Index:      vi.Index,
		IndexIdent: vi.IndexIdent,
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
