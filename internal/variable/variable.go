package variable

type VariableInfo struct {
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
		Position:   vi.Position,
		Size:       vi.Size,
		Typ:        vi.Typ,
		Const:      vi.Const,
		Len:        vi.Len,
		Index:      vi.Index,
		IndexIdent: vi.IndexIdent,
	}
}
