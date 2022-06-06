package variable

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
