package hwgenerator

import (
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go2async/internal/components"
	"go2async/internal/globalArguments"
	infoprinter "go2async/internal/infoPrinter"
	"go2async/pkg/variable"
	"io/ioutil"
	"os"
	"reflect"
	"strconv"
)

type Generator struct {
	components map[string]components.Component
	scopes     map[string]*components.Scope
	defs       *Defs

	peb *parseErrorBuilder
}

// current variable pos
var cvp = 0

func NewGenerator(intSize int) *Generator {
	components.SupportedTypes["int"] = intSize
	components.SupportedTypes["uint"] = intSize

	return &Generator{
		components: make(map[string]components.Component),
		scopes:     make(map[string]*components.Scope),
		defs:       NewDefs(intSize),
	}
}

func (g *Generator) GenerateFuncBlock(s *ast.AssignStmt, parent *components.Block) (fb components.BodyComponentType, err error) {
	if len(s.Lhs) > 1 {
		return nil, g.peb.NewParseError(s, errors.New("Expression lists are not allowed"))
	}

	lhsExpr := s.Lhs[0]
	rhsExpr := s.Rhs[0]

	/////////////////////////////////

	var lhsVar *variable.VariableInfo
	newVar := false

	if s.Tok == token.DEFINE { // :=
		newVar = true
		// get var after looking at RHS ... we need to determine type
	} else if s.Tok == token.ASSIGN { // =
		switch lhstype := lhsExpr.(type) {
		case *ast.Ident:
			lhsVar, err = parent.GetVariable(lhstype.Name)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		case *ast.IndexExpr:
			X, ok := lhstype.X.(*ast.Ident)
			if !ok {
				return nil, g.peb.NewParseError(s, errors.New("Lhs index expression X has to be an identifier"))
			}

			lhsVar, err = parent.GetVariable(X.Name)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			switch indexNode := lhstype.Index.(type) {
			case *ast.BasicLit:
				if indexNode.Kind != token.INT {
					return nil, g.peb.NewParseError(s, errors.New("Invalid indexing value kind '"+indexNode.Kind.String()+"'"))
				}
				lhsVar.Index = indexNode.Value
			case *ast.Ident:
				v, err := parent.GetVariable(indexNode.Name)
				if err != nil {
					return nil, g.peb.NewParseError(s, err)
				}

				lhsVar.IndexIdent = v
			default:
				return nil, g.peb.NewParseError(s, errors.New("Invalid indexing"))
			}
		default:
			return nil, g.peb.NewParseError(s, errors.New("Invalid lhs type '"+reflect.TypeOf(lhsExpr).String()+"'"))
		}
	} else { // ?
		return nil, g.peb.NewParseError(s, errors.New("Invalid token in assign statement (expected '=' or ':=')"))
	}

	switch rhsExpr := rhsExpr.(type) {
	case *ast.BasicLit:
		if newVar {
			typ := getTypeOfString(rhsExpr.Value)

			if lhsVar, err = parent.NewVariable(lhsExpr.(*ast.Ident).Name, typ, 1); err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		newFuncBlk := components.NewBinExprBlock("=", &components.OperandInfo{
			R: lhsVar,
			X: &variable.VariableInfo{
				Const: rhsExpr.Value,
				Size:  lhsVar.Size,
			},
		}, parent)

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk)

		return newFuncBlk, nil
	case *ast.Ident:
		v, err := parent.GetVariable(rhsExpr.Name)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}

		if newVar {
			if lhsVar, err = parent.NewVariable(lhsExpr.(*ast.Ident).Name, v.Typ, 1); err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		newFuncBlk := components.NewBinExprBlock("=", &components.OperandInfo{
			R: lhsVar,
			X: v,
		}, parent)

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk)

		return newFuncBlk, nil
	case *ast.IndexExpr:
		v, err := parent.GetVariable(rhsExpr.X.(*ast.Ident).Name)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}

		switch indexNode := rhsExpr.Index.(type) {
		case *ast.BasicLit:
			if indexNode.Kind != token.INT {
				return nil, g.peb.NewParseError(s, errors.New("Invalid indexing value kind '"+indexNode.Kind.String()+"'"))
			}
			v.Index = indexNode.Value
		case *ast.Ident:
			vn, err := parent.GetVariable(indexNode.Name)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			v.IndexIdent = vn
		default:
			return nil, g.peb.NewParseError(s, errors.New("Invalid indexing"))
		}

		if newVar {
			if lhsVar, err = parent.NewVariable(lhsExpr.(*ast.Ident).Name, v.Typ, 1); err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		newFuncBlk := components.NewBinExprBlock("=", &components.OperandInfo{
			R: lhsVar,
			X: v,
		}, parent)

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk)

		return newFuncBlk, nil

	case *ast.BinaryExpr:
		if newVar {
			// TODO: infer size first
			return nil, g.peb.NewParseError(s, errors.New("Cannot initialize variable with binary expression"))
		}

		tmpBlock, err := g.GenerateBlock([]ast.Stmt{}, false, parent, false)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}

		tmpVar, err := tmpBlock.NewVariable("__g2a_tempVar", lhsVar.Typ, lhsVar.Len)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}

		g.components[tmpBlock.ArchName()] = tmpBlock
		parent.AddComponent(tmpBlock)

		_, err = g.GenerateBinaryExpressionFuncBlock(tmpVar, rhsExpr, tmpBlock)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}

		newFuncBlk := components.NewBinExprBlock("=", &components.OperandInfo{
			R: lhsVar,
			X: tmpVar,
		}, tmpBlock)

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk)

		return newFuncBlk, nil
	case *ast.CallExpr:
		funcNameIdent, ok := rhsExpr.Fun.(*ast.Ident)
		if !ok {
			return nil, g.peb.NewParseError(s, errors.New("funtion name is not an identifier"))
		}
		funcName := funcNameIdent.Name

		funcIntf, err := parent.GetAndAssignFunctionInterface(funcName)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}

		paramList := []*variable.VariableInfo{}

		for i, fp := range rhsExpr.Args {
			funcParamIdent, ok := fp.(*ast.Ident)
			if !ok {
				return nil, g.peb.NewParseError(s, errors.New("Call expression parameters have to be an Identifier"))
			}

			vi, err := parent.GetVariable(funcParamIdent.Name)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			correspondingFuncParamVar, err := funcIntf.Parameters.GetVariableInfoAt(i)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			if vi.Typ != correspondingFuncParamVar.Typ {
				return nil, g.peb.NewParseError(s, errors.New("Call expression has type mismatch at parameter "+strconv.Itoa(i)))
			}
			paramList = append(paramList, vi)
		}

		if newVar {
			firstResult, _ := funcIntf.Results.GetVariableInfoAt(0)
			if lhsVar, err = parent.NewVariable(lhsExpr.(*ast.Ident).Name, firstResult.Typ, 1); err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		resultList := []*variable.VariableInfo{lhsVar}

		newFuncBlk, err := components.NewFuncBlock(paramList, resultList, funcIntf, parent)
		if err != nil {
			return nil, err
		}

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk)

		return newFuncBlk, nil

		// TOTO: Valid CallExpr: make block for expression

		//return nil, g.peb.NewParseError(s, errors.New("Call expression in rhs not supported yet!"))
	default:
		return nil, g.peb.NewParseError(s, errors.New("Expression "+reflect.TypeOf(rhsExpr).String()+" in rhs not supported!"))
	}
}

func (g *Generator) GenerateBinaryExpressionFuncBlock(result *variable.VariableInfo, be *ast.BinaryExpr, parent *components.Block) (fb *components.BinExprBlock, err error) {
	xexpr := be.X
	yexpr := be.Y

	var x, y *variable.VariableInfo

	operation := be.Op.String()
	if _, ok := components.SupportedOperations[operation]; !ok {
		return nil, g.peb.NewParseError(be, errors.New("Operation "+operation+" not supported"))
	}

	switch t := xexpr.(type) {
	case *ast.BinaryExpr:
		infoprinter.DebugPrintln("nexted binary expression left: ", t.X, t.Op.String(), t.Y)

		g.GenerateBinaryExpressionFuncBlock(result, t, parent)
		x = result
	case *ast.BasicLit:
		x = &variable.VariableInfo{
			Const: t.Value,
			Size:  result.Size,
		}
	case *ast.Ident:
		x, err = parent.GetVariable(t.Name)
		if err != nil {
			return nil, g.peb.NewParseError(be, err)
		}
	case *ast.IndexExpr:
		x, err = parent.GetVariable(t.X.(*ast.Ident).Name)
		if err != nil {
			return nil, g.peb.NewParseError(be, err)
		}

		switch indexNode := t.Index.(type) {
		case *ast.BasicLit:
			if indexNode.Kind != token.INT {
				return nil, g.peb.NewParseError(be, errors.New("Invalid indexing value kind '"+indexNode.Kind.String()+"'"))
			}
			x.Index = indexNode.Value
		case *ast.Ident:
			v, err := parent.GetVariable(t.X.(*ast.Ident).Name)
			if err != nil {
				return nil, g.peb.NewParseError(be, err)
			}

			x.IndexIdent = v
		default:
			return nil, g.peb.NewParseError(be, errors.New("Invalid indexing"))
		}
	default:
		ref := reflect.TypeOf(t)
		if ref == nil {
			return nil, g.peb.NewParseError(be, errors.New("Invalid type in binary expression: <nil>"))
		}
		return nil, g.peb.NewParseError(be, errors.New("Invalid type in binary expression: "+ref.String()))
	}

	switch t := yexpr.(type) {
	case *ast.BinaryExpr:
		infoprinter.DebugPrintln("nested binary expression right: ", t.X, t.Op.String(), t.Y)
		return nil, g.peb.NewParseError(be, errors.New("Binary expression in right side of binary expression not allowed"))
	case *ast.BasicLit:
		y = &variable.VariableInfo{
			Const: t.Value,
			Size:  result.Size,
		}
	case *ast.Ident:
		y, err = parent.GetVariable(t.Name)
		if err != nil {
			return nil, g.peb.NewParseError(be, err)
		}
	case *ast.IndexExpr:
		y, err = parent.GetVariable(t.X.(*ast.Ident).Name)
		if err != nil {
			return nil, g.peb.NewParseError(be, err)
		}

		switch indexNode := t.Index.(type) {
		case *ast.BasicLit:
			if indexNode.Kind != token.INT {
				return nil, g.peb.NewParseError(be, errors.New("Invalid indexing value kind '"+indexNode.Kind.String()+"'"))
			}
			y.Index = indexNode.Value
		case *ast.Ident:
			v, err := parent.GetVariable(indexNode.Name)
			if err != nil {
				return nil, g.peb.NewParseError(be, err)
			}

			y.IndexIdent = v
		default:
			return nil, g.peb.NewParseError(be, errors.New("Invalid indexing"))
		}
	default:
		ref := reflect.TypeOf(t)
		if ref == nil {
			return nil, g.peb.NewParseError(be, errors.New("Invalid type in binary expression: <nil>"))
		}
		return nil, g.peb.NewParseError(be, errors.New("Invalid type in binary expression: "+ref.String()))
	}

	infoprinter.DebugPrintln("generating func: ", result.Name, " = ", x.Name, " ", operation, " ", y.Name, " const: ", y.Const)

	newFuncBlk := components.NewBinExprBlock(operation, &components.OperandInfo{
		R: result,
		X: x,
		Y: y,
	}, parent)

	g.components[newFuncBlk.ArchName()] = newFuncBlk
	parent.AddComponent(newFuncBlk)

	return newFuncBlk, nil
}

func (g *Generator) GenerateSelectorBlock(be *ast.BinaryExpr, inverted bool, parent *components.Block) (c *components.SelectorBlock, err error) {
	xexpr := be.X
	yexpr := be.Y

	var x, y *variable.VariableInfo

	comp := be.Op.String()
	if _, ok := components.SupportedComperators[comp]; !ok {
		return nil, g.peb.NewParseError(be, errors.New("Invalid comperator "+comp))
	}

	switch t := xexpr.(type) {
	case *ast.BasicLit:
		x = &variable.VariableInfo{
			Const: t.Value,
		}
	case *ast.Ident:
		x, err = parent.GetVariable(t.Name)
		if err != nil {
			return nil, g.peb.NewParseError(be, err)
		}
	case *ast.IndexExpr:
		x, err = parent.GetVariable(t.X.(*ast.Ident).Name)
		if err != nil {
			return nil, g.peb.NewParseError(be, err)
		}

		switch indexNode := t.Index.(type) {
		case *ast.BasicLit:
			if indexNode.Kind != token.INT {
				return nil, g.peb.NewParseError(be, errors.New("Invalid indexing value kind '"+indexNode.Kind.String()+"'"))
			}
			x.Index = indexNode.Value
		case *ast.Ident:
			v, err := parent.GetVariable(indexNode.Name)
			if err != nil {
				return nil, g.peb.NewParseError(be, err)
			}

			x.IndexIdent = v
		default:
			return nil, g.peb.NewParseError(be, errors.New("Invalid indexing"))
		}
	default:
		ref := reflect.TypeOf(t)
		if ref == nil {
			return nil, g.peb.NewParseError(be, errors.New("Invalid type in binary expression: <nil>"))
		}
		return nil, g.peb.NewParseError(be, errors.New("Invalid type in binary expression: "+ref.String()))
	}

	switch t := yexpr.(type) {
	case *ast.BasicLit:
		y = &variable.VariableInfo{
			Const: t.Value,
			Size:  x.Size,
		}
	case *ast.Ident:
		y, err = parent.GetVariable(t.Name)
		if err != nil {
			return nil, g.peb.NewParseError(be, err)
		}
	case *ast.IndexExpr:
		y, err = parent.GetVariable(t.X.(*ast.Ident).Name)
		if y == nil {
			return nil, g.peb.NewParseError(be, err)
		}

		switch indexNode := t.Index.(type) {
		case *ast.BasicLit:
			if indexNode.Kind != token.INT {
				return nil, g.peb.NewParseError(be, errors.New("Invalid indexing value kind '"+indexNode.Kind.String()+"'"))
			}
			y.Index = indexNode.Value
		case *ast.Ident:
			v, err := parent.GetVariable(indexNode.Name)
			if err != nil {
				return nil, g.peb.NewParseError(be, err)
			}

			y.IndexIdent = v
		default:
			return nil, g.peb.NewParseError(be, errors.New("Invalid indexing"))
		}
	default:
		ref := reflect.TypeOf(t)
		if ref == nil {
			return nil, g.peb.NewParseError(be, errors.New("Invalid type in binary expression: <nil>"))
		}
		return nil, g.peb.NewParseError(be, errors.New("Invalid type in binary expression: "+ref.String()))
	}

	if x.Const != "" {
		x.Size = y.Size
	}

	return components.NewSelectorBlock(comp, &components.OperandInfo{
		X: x,
		Y: y,
	}, inverted, parent), nil
}

func (g *Generator) GenerateIfBlock(is *ast.IfStmt, parent *components.Block) (fb *components.IfBlock, err error) {
	var cond *components.SelectorBlock

	switch x := is.Cond.(type) {
	case *ast.BinaryExpr:
		cond, err = g.GenerateSelectorBlock(x, false, parent)
		if err != nil {
			return nil, g.peb.NewParseError(is, err)
		}
	case *ast.ParenExpr:
		xBin, ok := x.X.(*ast.BinaryExpr)
		if !ok {
			ref := reflect.TypeOf(x.X)
			if ref == nil {
				return nil, g.peb.NewParseError(is, errors.New("Only binary expression in if condition allowed found <nil> in parenthesis"))
			}
			return nil, g.peb.NewParseError(is, errors.New("Only binary expression in if condition allowed found "+ref.String()+" in parenthesis"))
		}
		cond, err = g.GenerateSelectorBlock(xBin, false, parent)
		if err != nil {
			return nil, g.peb.NewParseError(is, err)
		}
	default:
		ref := reflect.TypeOf(x)
		if ref == nil {
			return nil, g.peb.NewParseError(is, errors.New("Only binary expression in if condition allowed found <nil>"))
		}
		return nil, g.peb.NewParseError(is, errors.New("Only binary expression in if condition allowed found "+ref.String()))
	}

	thenBody, err := g.GenerateBlock(is.Body.List, false, parent, false)
	if err != nil {
		return nil, g.peb.NewParseError(is, err)
	}

	var elseBody components.BodyComponentType
	elseBody = components.NewBinExprBlock("NOP", &components.OperandInfo{
		R: &variable.VariableInfo{
			Position: 0,
			Size:     8,
		},
		X: &variable.VariableInfo{
			Position: 0,
			Size:     8,
		},
	}, parent)
	if is.Else != nil {
		elseBody, err = g.GenerateBlock(is.Else.(*ast.BlockStmt).List, false, parent, false)
		if err != nil {
			return nil, g.peb.NewParseError(is, err)
		}
	}

	g.components[cond.ArchName()] = cond
	g.components[thenBody.ArchName()] = thenBody
	g.components[elseBody.ArchName()] = elseBody

	newIfBlk := components.NewIfBlock(cond, thenBody, elseBody, parent)
	g.components[newIfBlk.ArchName()] = newIfBlk
	parent.AddComponent(newIfBlk)

	return newIfBlk, nil
}

func (g *Generator) GenerateLoopBlock(fs *ast.ForStmt, parent *components.Block) (fb *components.LoopBlock, err error) {
	var cond *components.SelectorBlock

	switch x := fs.Cond.(type) {
	case *ast.BinaryExpr:
		cond, err = g.GenerateSelectorBlock(x, true, parent)
		if err != nil {
			return nil, g.peb.NewParseError(fs, err)
		}
	case *ast.ParenExpr:
		xBin, ok := x.X.(*ast.BinaryExpr)
		if !ok {
			ref := reflect.TypeOf(x.X)
			if ref == nil {
				return nil, g.peb.NewParseError(fs, errors.New("Only binary expression in for condition allowed found <nil> in parenthesis"))
			}
			return nil, g.peb.NewParseError(fs, errors.New("Only binary expression in for condition allowed found "+reflect.TypeOf(x.X).String()+" in parenthesis"))
		}
		cond, err = g.GenerateSelectorBlock(xBin, false, parent)
		if err != nil {
			return nil, g.peb.NewParseError(fs, err)
		}
	default:
		ref := reflect.TypeOf(x)
		if ref == nil {
			return nil, g.peb.NewParseError(fs, errors.New("Only binary expression in for condition allowed found <nil>"))
		}
		return nil, g.peb.NewParseError(fs, errors.New("Only binary expression in for condition allowed found "+reflect.TypeOf(x).String()))
	}

	body, err := g.GenerateBlock(fs.Body.List, false, parent, false)
	if err != nil {
		return nil, g.peb.NewParseError(fs, err)
	}

	g.components[cond.ArchName()] = cond
	g.components[body.ArchName()] = body

	newForBlk := components.NewLoopBlock(cond, body, parent)
	g.components[newForBlk.ArchName()] = newForBlk
	parent.AddComponent(newForBlk)

	return newForBlk, nil
}

func getTypeOfString(x string) string {
	if _, err := strconv.Atoi(x); err == nil {
		return "uint8"
	}

	return "string"
}

func (g *Generator) GenerateBodyBlock(s ast.Stmt, parent *components.Block) (c components.BodyComponentType, err error) {
	switch sType := s.(type) {
	case *ast.DeclStmt:
		decl, ok := sType.Decl.(*ast.GenDecl)
		if !ok {
			return nil, g.peb.NewParseError(s, errors.New("Invalid declaration type!"))
		} else {
			if decl.Tok != token.VAR {
				return nil, g.peb.NewParseError(s, errors.New("Only var declaration allowed!"))
			}
		}

		for _, spec := range decl.Specs[0].(*ast.ValueSpec).Names {
			switch declType := decl.Specs[0].(*ast.ValueSpec).Type.(type) {
			case *ast.Ident:
				if _, err := parent.NewVariable(spec.Name, declType.Name, 1); err != nil {
					return nil, g.peb.NewParseError(s, err)
				}
			case *ast.ArrayType:
				len, ok := declType.Len.(*ast.BasicLit)
				if !ok {
					return nil, g.peb.NewParseError(s, errors.New("Index in array declaration as to be a basic lit of kind INT"))
				} else {
					if len.Kind != token.INT {
						return nil, g.peb.NewParseError(s, errors.New("Index in array declaration as to be a basic lit of kind INT"))
					}
				}
				elt, ok := declType.Elt.(*ast.Ident)
				if !ok {
					return nil, g.peb.NewParseError(s, errors.New("Invalid array declaration"))
				}

				leni, err := strconv.Atoi(len.Value)
				if err != nil {
					return nil, g.peb.NewParseError(s, err)
				}

				if _, err := parent.NewVariable(spec.Name, elt.Name, leni); err != nil {
					return nil, g.peb.NewParseError(s, err)
				}
			default:
				return nil, g.peb.NewParseError(s, errors.New("Invalid declaration"))
			}
		}

		return nil, nil
	case *ast.AssignStmt:
		return g.GenerateFuncBlock(sType, parent)
	case *ast.ForStmt:
		return g.GenerateLoopBlock(sType, parent)
	case *ast.IfStmt:
		return g.GenerateIfBlock(sType, parent)
	case *ast.ReturnStmt:
		return nil, g.peb.NewParseError(s, errors.New("Return statements only allowed at the end of function!"))
	case *ast.BlockStmt:
		return g.GenerateBlock(sType.List, false, parent, true)
	default:
		return nil, g.peb.NewParseError(s, errors.New("Invalid statement "+reflect.TypeOf(sType).String()))
	}
}

func (g *Generator) GenerateBlock(stmts []ast.Stmt, toplevelStatement bool, parent *components.Block, addComponentToParent bool) (b *components.Block, err error) {
	b = components.NewBlock(toplevelStatement, parent)

	for _, s := range stmts {
		newComponent, err := g.GenerateBodyBlock(s, b)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}
		if newComponent == nil {
			continue
		}

		/* g.components[newComponent.ArchName()] = newComponent
		b.AddComponent(newComponent) */
	}

	if addComponentToParent {
		g.components[b.ArchName()] = b
		parent.AddComponent(b)
	}

	return b, nil
}

func (g *Generator) parseVariableExpression(expr ast.Expr) (string, int, error) {
	switch fieldType := expr.(type) {
	case *ast.Ident:
		return fieldType.Name, 1, nil
	case *ast.ArrayType:
		fieldTypeLenBasicLit, ok := fieldType.Len.(*ast.BasicLit)
		if !ok {
			return "", -1, g.peb.NewParseError(expr, errors.New("Slices are not supported: missing array length"))
		}

		len, err := strconv.Atoi(fieldTypeLenBasicLit.Value)
		if err != nil {
			return "", -1, g.peb.NewParseError(expr, err)
		}

		return fieldType.Elt.(*ast.Ident).Name, len, nil
	default:
		return "", -1, g.peb.NewParseError(expr, errors.New("Invalid variable expression"))
	}
}

func (g *Generator) GenerateScope(f *ast.FuncDecl) (s *components.Scope, err error) {
	params := make(map[string]*variable.VariableInfo)

	// only relevant for variable handling
	paramDummyBlock := components.NewParamDummyBlock(params)

	for _, field := range f.Type.Params.List {
		for _, param := range field.Names {
			switch fieldType := field.Type.(type) {
			case *ast.Ident, *ast.ArrayType:
				typeName, len, err := g.parseVariableExpression(fieldType)
				if err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				np, err := paramDummyBlock.NewVariable(param.Name, typeName, len)
				if err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				params[param.Name] = np
			case *ast.FuncType:
				newFuncIntf := components.NewFuncInterface(param.Name)

				if *globalArguments.Debug {
					fmt.Printf("Adding functionInterface for function '%s'\n", newFuncIntf.Name)
				}

				for _, p := range fieldType.Params.List {
					typeName, leng, err := g.parseVariableExpression(p.Type)
					if err != nil {
						return nil, g.peb.NewParseError(f, err)
					}

					if len(p.Names) > 0 {
						for _, n := range p.Names {
							name := n.Name
							newFuncIntf.Parameters.AddVariable(name, typeName, leng)
						}
					} else {
						newFuncIntf.Parameters.AddVariable("", typeName, leng)
					}

				}

				for _, p := range fieldType.Results.List {
					typeName, leng, err := g.parseVariableExpression(p.Type)
					if err != nil {
						return nil, g.peb.NewParseError(f, err)
					}

					if len(p.Names) > 0 {
						for _, n := range p.Names {
							name := n.Name
							newFuncIntf.Results.AddVariable(name, typeName, leng)
						}
					} else {
						newFuncIntf.Results.AddVariable("", typeName, leng)
					}

				}

				if *globalArguments.Debug {
					fmt.Printf("Created new external interface declaration for function %s; Param len %d; Result len %d\n", newFuncIntf.Name, newFuncIntf.Parameters.GetSize(), newFuncIntf.Results.GetSize())
				}

				if err := paramDummyBlock.AddFunctionInterface(newFuncIntf); err != nil {
					return nil, g.peb.NewParseError(f, err)
				}
			default:
				return nil, g.peb.NewParseError(f, errors.New("Type '"+reflect.TypeOf(fieldType).String()+"' is not supported in function param list"))
			}
		}
	}

	if len(params) == 0 {
		return nil, g.peb.NewParseError(f, errors.New("At least one function parameter expected"))
	}

	fields := len(f.Body.List)
	if fields < 2 {
		return nil, g.peb.NewParseError(f, errors.New("At least one top-level statement + return expected"))
	}

	block, err := g.GenerateBlock(f.Body.List[0:fields-1], false, paramDummyBlock, true)
	if err != nil {
		return nil, g.peb.NewParseError(f, err)
	}
	block.OutputSize = block.ScopedVariables().GetSize()

	rs, ok := f.Body.List[fields-1].(*ast.ReturnStmt)
	if !ok {
		return nil, g.peb.NewParseError(f, errors.New("Missing return statement at the end"))
	}

	returnPositions := []*variable.VariableInfo{}
	for _, res := range rs.Results {
		switch x := res.(type) {
		case *ast.Ident:
			vi, err := block.GetVariable(x.Name)
			if err != nil {
				return nil, g.peb.NewParseError(f, err)
			}

			returnPositions = append(returnPositions, vi)
		case *ast.IndexExpr:
			vi, err := block.GetVariable(x.X.(*ast.Ident).Name)
			if err != nil {
				return nil, g.peb.NewParseError(f, err)
			}

			switch indexType := x.Index.(type) {
			case *ast.BasicLit:
				vi.Index = indexType.Value
			case *ast.Ident:
				return nil, g.peb.NewParseError(f, errors.New("No identifier in index in returns allowed"))
			default:
				return nil, g.peb.NewParseError(f, errors.New("Invalid indexType in return"))
			}

			returnPositions = append(returnPositions, vi)
		default:
			return nil, g.peb.NewParseError(f, errors.New("Invalid expression in return"))
		}
	}

	s = components.NewScope(f.Name.Name, block, params, returnPositions)

	g.components[block.ArchName()] = block
	g.scopes[s.ArchName()] = s
	return s, nil
}

func (g *Generator) ParseGoFile(file string) error {
	fset := token.NewFileSet()
	g.peb = NewParseErrorBuilder(fset)

	b, err := ioutil.ReadFile(file)
	if err != nil {
		return err
	}

	f, err := parser.ParseFile(fset, file, string(b), 0)
	if err != nil {
		return err
	}

	if len(f.Imports) > 0 {
		return g.peb.NewParseError(f, errors.New("No imports allowed"))
	}

	//ast.Print(fset, f)
	for _, decl := range f.Decls {
		switch x := decl.(type) {
		case *ast.FuncDecl:
			_, err := g.GenerateScope(x)
			if err != nil {
				return err
			}
		default:
			return g.peb.NewParseError(x, errors.New(reflect.TypeOf(x).String()+" not allowed! Expected fucntion declaration"))
		}
	}

	return nil
}

func (g *Generator) GenerateVHDL() string {
	ret := ""

	entityTracker := map[string]bool{}

	for _, c := range g.components {
		if _, ok := entityTracker[c.EntityName()]; !ok {
			entityTracker[c.EntityName()] = true

			// prefix entity if not already added
			ret = c.Entity() + ret
		}

		ret += c.Architecture()
	}

	for sn, s := range g.scopes {
		for _, externalIntf := range s.Block.ExternalInterfaces {
			g.defs.ScopeProperties[externalIntf.Name] = &ScopeProperty{
				paramSize:  externalIntf.Parameters.GetSize(),
				returnSize: externalIntf.Results.GetSize(),
			}
		}

		ret = s.Entity() + ret

		ps, rs := 0, 0
		for _, s := range s.Params {
			ps += s.Size * s.Len
		}

		for _, v := range s.Params {
			fmt.Println(sn, ": Param ", v.String())
		}

		for i, s := range s.ReturnVars {
			fmt.Println(sn, ": ReturnVar(", i, ") ", s.String())

			rs += s.Size * s.Len
		}

		g.defs.ScopeProperties[sn] = &ScopeProperty{
			paramSize:  s.Block.GetVariableSize(),
			returnSize: rs,
		}
		ret += s.Architecture()
	}

	return g.defs.GetDefs() + ret
}

func (g *Generator) SaveVHDL(file *os.File) error {
	file.Truncate(0)
	_, err := file.Write([]byte(g.GenerateVHDL()))
	if err != nil {
		return err
	}
	return nil
}
