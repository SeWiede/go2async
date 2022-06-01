package hwgenerator

import (
	"go/ast"
	"go/token"
	"strconv"
)

type parseError struct {
	underlyingError error

	position token.Position
}

func NewParseError(fset *token.FileSet, node ast.Node, err error) *parseError {
	return &parseError{
		underlyingError: err,
		position:        fset.Position(node.Pos()),
	}
}

func (pe *parseError) Error() string {
	fName := ""
	fLine := "-"
	fCol := "-"

	fName = pe.position.Filename
	fLine = strconv.Itoa(pe.position.Line)
	fCol = strconv.Itoa(pe.position.Column)

	return fName + ":" + fLine + " " + fCol + " - " + "\n" + pe.underlyingError.Error()
}

func getParseError(fset *token.FileSet, node ast.Node, err error) error {
	return NewParseError(fset, node, err)
}

type parseErrorBuilder struct {
	fset *token.FileSet
}

func NewParseErrorBuilder(fset *token.FileSet) *parseErrorBuilder {
	return &parseErrorBuilder{
		fset: fset,
	}
}

func (peb *parseErrorBuilder) NewParseError(node ast.Node, err error) error {
	return NewParseError(peb.fset, node, err)
}
