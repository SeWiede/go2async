package hwgenerator

import (
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go2async/internal/components"
	infoPrinter "go2async/internal/infoPrinter"
	"go2async/internal/variable"
	"io/ioutil"
	"os"
	"reflect"
	"strconv"
)

type Generator struct {
	components map[string]components.Component
	scopes     map[string]*components.Scope

	functions map[string]variable.VariableDef

	defs *Defs

	peb *parseErrorBuilder
}

// current variable pos
var cvp = 0

func NewGenerator(intSize int) *Generator {
	variable.SupportedTypes["int"] = intSize
	variable.SupportedTypes["uint"] = intSize

	return &Generator{
		components: make(map[string]components.Component),
		scopes:     make(map[string]*components.Scope),

		functions: make(map[string]variable.VariableDef),

		defs: NewDefs(intSize),
	}
}

func (g *Generator) HandleAssignmentStmt(s *ast.AssignStmt, parent components.BlockType) (fb components.BodyComponentType, err error) {
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
				lhsVar.Index_ = indexNode.Value
			case *ast.Ident:
				v, err := parent.GetVariable(indexNode.Name)
				if err != nil {
					return nil, g.peb.NewParseError(s, err)
				}

				lhsVar.IndexIdent_ = v
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

			varDecl := &variable.VariableTypeDecl{
				Name_: lhsExpr.(*ast.Ident).Name,
				Typ_:  typ,
				Len_:  1,
			}

			// TODO: REMOVE
			/* if lhsVar, err = parent.NewVariable(varDecl); err != nil {
				return nil, g.peb.NewParseError(s, err)
			} */

			if lhsVar, err = variable.NewLocalVariable(varDecl); err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		constVar, err := variable.MakeConst(rhsExpr.Value, lhsVar.Typ_)
		if err != nil {
			return nil, err
		}

		newFuncBlk, err := components.NewBinExprBlock("=", &components.OperandInfo{
			R: lhsVar,
			X: constVar,
		}, parent)
		if err != nil {
			return nil, err
		}

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk)

		return newFuncBlk, nil
	case *ast.Ident:
		v, err := parent.GetVariable(rhsExpr.Name)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}

		if newVar {
			varDecl := &variable.VariableTypeDecl{
				Name_: lhsExpr.(*ast.Ident).Name,
				Typ_:  v.Typ_,
				Len_:  1,
			}

			// TODO: REMOVE
			/* if lhsVar, err = parent.NewVariable(varDecl); err != nil {
				return nil, g.peb.NewParseError(s, err)
			} */

			if lhsVar, err = variable.NewLocalVariable(varDecl); err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		newFuncBlk, err := components.NewBinExprBlock("=", &components.OperandInfo{
			R: lhsVar,
			X: v,
		}, parent)
		if err != nil {
			return nil, err
		}

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
			v.Index_ = indexNode.Value
		case *ast.Ident:
			vn, err := parent.GetVariable(indexNode.Name)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			v.IndexIdent_ = vn
		default:
			return nil, g.peb.NewParseError(s, errors.New("Invalid indexing"))
		}

		if newVar {
			varDecl := &variable.VariableTypeDecl{
				Name_: lhsExpr.(*ast.Ident).Name,
				Typ_:  v.Typ_,
				Len_:  1,
			}

			// TODO: REMOVE
			/* if lhsVar, err = parent.NewVariable(varDecl); err != nil {
				return nil, g.peb.NewParseError(s, err)
			} */

			if lhsVar, err = variable.NewLocalVariable(varDecl); err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		newFuncBlk, err := components.NewBinExprBlock("=", &components.OperandInfo{
			R: lhsVar,
			X: v,
		}, parent)
		if err != nil {
			return nil, err
		}

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk)

		return newFuncBlk, nil

	case *ast.BinaryExpr:
		if newVar {
			// TODO: infer size first
			return nil, g.peb.NewParseError(s, errors.New("Cannot initialize variable with binary expression"))
		}

		/*
			TODO: WORK IN PROGRESS
			tmpBlock, err := g.GenerateBlock([]ast.Stmt{}, false, parent, false)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			varDecl := &variable.VariableTypeDecl{
				Name_: "__g2a_tempVar",
				Typ_:  lhsVar.Typ_,
				Len_:  lhsVar.Len_,
			}

			// TODO: REMOVE
			// tmpVar, err := tmpBlock.NewVariable(varDecl)
			// if err != nil {
			// 	return nil, g.peb.NewParseError(s, err)
			// }

			tmpVar, err := variable.NewLocalVariable(varDecl)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			g.components[tmpBlock.ArchName()] = tmpBlock
			parent.AddComponent(tmpBlock)
		*/

		newFuncBlock, err := g.GenerateBinaryExpressionBlock(lhsVar, rhsExpr, parent)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}

		/* newFuncBlk, err := components.NewBinExprBlock("=", &components.OperandInfo{
			R: lhsVar,
			X: tmpVar,
		}, parent)
		if err != nil {
			return nil, err
		}

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk) */

		// Components added previous call

		return newFuncBlock, nil
	case *ast.CallExpr:
		funcNameIdent, ok := rhsExpr.Fun.(*ast.Ident)
		if !ok {
			return nil, g.peb.NewParseError(s, errors.New("funtion name is not an identifier"))
		}
		funcName := funcNameIdent.Name

		isPackageFunction := false

		var funcIntf variable.VariableDef

		// Prioritize functionPointer params - of course
		funcIntf, err = parent.GetAndAssignFunctionInterface(funcName)
		if err != nil {
			funcIntf, ok = g.functions[funcName]
			if ok {
				infoPrinter.DebugPrintf("func '%s' is a function in the package\n", funcName)
				isPackageFunction = true
			} else {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		paramsResults := variable.NewFuncIntf()

		for i, fp := range rhsExpr.Args {
			funcParamIdent, ok := fp.(*ast.Ident)
			if !ok {
				return nil, g.peb.NewParseError(s, errors.New("Call expression parameters have to be an Identifier"))
			}

			vi, err := parent.GetVariable(funcParamIdent.Name)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			correspondingFuncParamVar, err := funcIntf.FuncIntf().Parameters.GetVariableInfoAt(i)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			if vi.Typ_ != correspondingFuncParamVar.Typ_ {
				return nil, g.peb.NewParseError(s, errors.New("Call expression has type mismatch at parameter "+strconv.Itoa(i)))
			}

			if _, err := paramsResults.Parameters.AddVariable(vi); err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		if newVar {
			firstResult, err := funcIntf.FuncIntf().Results.GetVariableInfoAt(0)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			varDecl := &variable.VariableTypeDecl{
				Name_: lhsExpr.(*ast.Ident).Name,
				Typ_:  firstResult.Typ_,
				Len_:  1,
			}

			// TODO: REMOVE
			/* if lhsVar, err = parent.NewVariable(varDecl); err != nil {
				return nil, g.peb.NewParseError(s, err)
			} */

			if lhsVar, err = variable.NewLocalVariable(varDecl); err != nil {
				return nil, g.peb.NewParseError(s, err)
			}
		}

		if _, err := paramsResults.Results.AddVariable(lhsVar); err != nil {
			return nil, g.peb.NewParseError(s, err)
		}

		infoPrinter.DebugPrintf("Function params of '%s' were of correct type\n", funcIntf.Name())

		var newFuncBlk components.BodyComponentType
		if !isPackageFunction {
			newFuncBlk, err = components.NewFuncBlock(paramsResults, funcIntf, parent)
			if err != nil {
				return nil, err
			}
		} else {
			newFuncBlk, err = components.NewCallBlock(paramsResults, funcIntf, parent)
			if err != nil {
				return nil, err
			}
		}

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk)

		return newFuncBlk, nil
	default:
		return nil, g.peb.NewParseError(s, errors.New("Expression "+reflect.TypeOf(rhsExpr).String()+" in rhs not supported!"))
	}
}

func (g *Generator) GenerateBinaryExpressionBlock(result *variable.VariableInfo, be *ast.BinaryExpr, parent components.BlockType) (fb *components.BinExprBlock, err error) {
	xexpr := be.X
	yexpr := be.Y

	var x, y *variable.VariableInfo

	operation := be.Op.String()
	if _, ok := components.SupportedOperations[operation]; !ok {
		return nil, g.peb.NewParseError(be, errors.New("Operation "+operation+" not supported"))
	}

	switch t := xexpr.(type) {
	case *ast.BinaryExpr:
		infoPrinter.DebugPrintln("nested binary expression ", result.Name_, " = ", t.X, t.Op.String(), t.Y)

		g.GenerateBinaryExpressionBlock(result, t, parent)
		/*newFuncBlk, err := g.GenerateBinaryExpressionBlock(result, t, parent)
		if err != nil {
			return nil, err
		}

		g.components[newFuncBlk.ArchName()] = newFuncBlk
		parent.AddComponent(newFuncBlk)*/

		// Component is generated at the end of function for this recursive call!

		x = result
	case *ast.BasicLit:
		x, err = variable.MakeConst(t.Value, result.Typ_)
		if err != nil {
			return nil, err
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
			x.Index_ = indexNode.Value
		case *ast.Ident:
			v, err := parent.GetVariable(t.X.(*ast.Ident).Name)
			if err != nil {
				return nil, g.peb.NewParseError(be, err)
			}

			x.IndexIdent_ = v
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
		infoPrinter.DebugPrintln("nested binary expression right: ", t.X, t.Op.String(), t.Y)
		return nil, g.peb.NewParseError(be, errors.New("Binary expression in right side of binary expression not allowed"))
	case *ast.BasicLit:
		y, err = variable.MakeConst(t.Value, result.Typ_)
		if err != nil {
			return nil, err
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
			y.Index_ = indexNode.Value
		case *ast.Ident:
			v, err := parent.GetVariable(indexNode.Name)
			if err != nil {
				return nil, g.peb.NewParseError(be, err)
			}

			y.IndexIdent_ = v
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

	infoPrinter.DebugPrintln("generating func: ", result.Name_, " = ", x.Name_, " ", operation, " ", y.Name_, " const: ", y.Const_)

	newFuncBlk, err := components.NewBinExprBlock(operation, &components.OperandInfo{
		R: result,
		X: x,
		Y: y,
	}, parent)
	if err != nil {
		return nil, err
	}

	g.components[newFuncBlk.ArchName()] = newFuncBlk
	parent.AddComponent(newFuncBlk)

	return newFuncBlk, nil
}

func (g *Generator) GenerateSelectorBlock(be *ast.BinaryExpr, inverted bool, holdOffConnections bool, parent components.BlockType) (c *components.SelectorBlock, err error) {
	xexpr := be.X
	yexpr := be.Y

	var x, y *variable.VariableInfo

	comp := be.Op.String()
	if _, ok := components.SupportedComperators[comp]; !ok {
		return nil, g.peb.NewParseError(be, errors.New("Invalid comperator "+comp))
	}

	switch t := xexpr.(type) {
	case *ast.BasicLit:
		// TODO: determine size
		x, err = variable.MakeConst(t.Value, "uint8")
		if err != nil {
			return nil, err
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
			x.Index_ = indexNode.Value
		case *ast.Ident:
			v, err := parent.GetVariable(indexNode.Name)
			if err != nil {
				return nil, g.peb.NewParseError(be, err)
			}

			x.IndexIdent_ = v
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
		y, err = variable.MakeConst(t.Value, x.Typ_)
		if err != nil {
			return nil, err
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
			y.Index_ = indexNode.Value
		case *ast.Ident:
			v, err := parent.GetVariable(indexNode.Name)
			if err != nil {
				return nil, g.peb.NewParseError(be, err)
			}

			y.IndexIdent_ = v
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

	if x.Const_ != "" {
		x.Size_ = y.Size_
	}

	return components.NewSelectorBlock(comp, &components.OperandInfo{
		X: x,
		Y: y,
	}, inverted, holdOffConnections, parent), nil
}

func (g *Generator) GenerateIfBlock(is *ast.IfStmt, parent components.BlockType) (fb *components.IfBlock, err error) {
	var cond *components.SelectorBlock

	newIfBlk := components.NewIfBlock(parent)

	switch x := is.Cond.(type) {
	case *ast.BinaryExpr:
		cond, err = g.GenerateSelectorBlock(x, false, false, newIfBlk)
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
		cond, err = g.GenerateSelectorBlock(xBin, false, false, newIfBlk)
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

	thenBody, err := g.GenerateBlock(is.Body.List, false, true, newIfBlk, false)
	if err != nil {
		return nil, g.peb.NewParseError(is, err)
	}

	var elseBody components.BodyComponentType
	elseBody, err = components.NewBinExprBlock("NOP", &components.OperandInfo{
		R: &variable.VariableInfo{
			Position_: 0,
			Size_:     8,
		},
		X: &variable.VariableInfo{
			Position_: 0,
			Size_:     8,
		},
	}, parent)
	if err != nil {
		return nil, err
	}

	if is.Else != nil {
		elseBody, err = g.GenerateBlock(is.Else.(*ast.BlockStmt).List, false, true, newIfBlk, false)
		if err != nil {
			return nil, g.peb.NewParseError(is, err)
		}
	}

	newIfBlk.AssignBodyComponents(cond, thenBody, elseBody)

	g.components[cond.ArchName()] = cond
	g.components[thenBody.ArchName()] = thenBody
	g.components[elseBody.ArchName()] = elseBody

	//newIfBlk := components.NewIfBlock(cond, thenBody, elseBody, parent)
	g.components[newIfBlk.ArchName()] = newIfBlk
	parent.AddComponent(newIfBlk)

	return newIfBlk, nil
}

func (g *Generator) GenerateLoopBlock(fs *ast.ForStmt, parent components.BlockType) (fb *components.LoopBlock, err error) {
	newForBlk := components.NewLoopBlock(parent)

	var cond *components.SelectorBlock

	switch x := fs.Cond.(type) {
	case *ast.BinaryExpr:
		cond, err = g.GenerateSelectorBlock(x, true, true, newForBlk)
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

		cond, err = g.GenerateSelectorBlock(xBin, true, true, newForBlk)
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

	body, err := g.GenerateBlock(fs.Body.List, false, true, newForBlk, false)
	if err != nil {
		return nil, g.peb.NewParseError(fs, err)
	}

	g.components[cond.ArchName()] = cond
	g.components[body.ArchName()] = body

	newForBlk.AssignBodyComponents(cond, body)

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

func (g *Generator) GenerateBodyBlock(s ast.Stmt, parent components.BlockType) (c components.BodyComponentType, err error) {
	switch sType := s.(type) {
	case *ast.DeclStmt:
		decl, ok := sType.Decl.(*ast.GenDecl)
		if !ok {
			return nil, g.peb.NewParseError(s, errors.New("Invalid declaration type!"))
		} else {
			if decl.Tok != token.VAR {
				return nil, g.peb.NewParseError(s, errors.New("Only var declaration supported!"))
			}
		}

		for _, spec := range decl.Specs[0].(*ast.ValueSpec).Names {
			switch declType := decl.Specs[0].(*ast.ValueSpec).Type.(type) {
			case *ast.Ident:
				if _, err := parent.NewScopeVariable(&variable.VariableTypeDecl{
					Name_: spec.Name,
					Typ_:  declType.Name,
					Len_:  1,
				}); err != nil {
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

				if _, err := parent.NewScopeVariable(&variable.VariableTypeDecl{
					Name_: spec.Name,
					Typ_:  elt.Name,
					Len_:  leni,
				}); err != nil {
					return nil, g.peb.NewParseError(s, err)
				}
			default:
				return nil, g.peb.NewParseError(s, errors.New("Invalid declaration"))
			}
		}

		return nil, nil
	case *ast.AssignStmt:
		return g.HandleAssignmentStmt(sType, parent)
	case *ast.ForStmt:
		return g.GenerateLoopBlock(sType, parent)
	case *ast.IfStmt:
		return g.GenerateIfBlock(sType, parent)
	case *ast.ReturnStmt:
		return nil, g.peb.NewParseError(s, errors.New("Return statements only allowed at the end of function!"))
	case *ast.BlockStmt:
		return g.GenerateBlock(sType.List, false, false, parent, true)
	default:
		return nil, g.peb.NewParseError(s, errors.New("Invalid statement "+reflect.TypeOf(sType).String()))
	}
}

func (g *Generator) GenerateBlock(stmts []ast.Stmt, toplevelStatement bool, KeepVariableOwnership bool, parent components.BlockType, addComponentToParent bool) (b *components.Block, err error) {
	b = components.NewBlock(toplevelStatement, KeepVariableOwnership, parent)

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

func (g *Generator) parseVariableFieldList(fl *ast.FieldList) ([]*variable.VariableTypeDecl, error) {
	ret := []*variable.VariableTypeDecl{}

	infoPrinter.DebugPrintf("Parsing variableFieldList\n")

	for _, param := range fl.List {
		if len(param.Names) > 0 {
			for _, p := range param.Names {
				paramVar, err := g.parseVariableExpression(param.Type)
				if err != nil {
					return nil, err
				}

				paramVar.Name_ = p.Name

				infoPrinter.DebugPrintf("Got var %s len %d type %s\n", paramVar.Name_, paramVar.Len_, paramVar.Typ_)

				ret = append(ret, paramVar)
			}
		} else {
			paramVar, err := g.parseVariableExpression(param.Type)
			if err != nil {
				return nil, err
			}

			infoPrinter.DebugPrintf("Got var %s len %d type %s\n", paramVar.Name_, paramVar.Len_, paramVar.Typ_)
			ret = append(ret, paramVar)
		}
	}

	infoPrinter.DebugPrintf("Done parsing variableFieldList\n")

	return ret, nil
}

func (g *Generator) parseVariableExpression(expr ast.Expr) (*variable.VariableTypeDecl, error) {
	switch fieldType := expr.(type) {
	case *ast.Ident:
		return &variable.VariableTypeDecl{
			Typ_: fieldType.Name,
			Len_: 1,
		}, nil
	case *ast.ArrayType:
		fieldTypeLenBasicLit, ok := fieldType.Len.(*ast.BasicLit)
		if !ok {
			return nil, g.peb.NewParseError(expr, errors.New("Slices are not supported: missing array length"))
		}

		len, err := strconv.Atoi(fieldTypeLenBasicLit.Value)
		if err != nil {
			return nil, g.peb.NewParseError(expr, err)
		}

		return &variable.VariableTypeDecl{
			Typ_: fieldType.Elt.(*ast.Ident).Name,
			Len_: len,
		}, nil
	case *ast.FuncType:
		funcIntf := variable.NewFuncIntf()

		listDecl, err := g.parseVariableFieldList(fieldType.Params)
		if err != nil {
			return nil, err
		}

		for _, ld := range listDecl {
			_, err := funcIntf.Parameters.AddVariable(ld)
			if err != nil {
				return nil, err
			}
		}

		listDecl, err = g.parseVariableFieldList(fieldType.Results)
		if err != nil {
			return nil, err
		}

		for _, ld := range listDecl {
			_, err := funcIntf.Results.AddVariable(ld)
			if err != nil {
				return nil, err
			}
		}

		return &variable.VariableTypeDecl{
			FuncIntf_: funcIntf,
		}, nil
	default:
		return nil, g.peb.NewParseError(expr, errors.New("Invalid variable expression"))
	}
}

func (g *Generator) GenerateScope(f *ast.FuncDecl) (s *components.Scope, err error) {
	params := make(map[string]*variable.VariableInfo)

	// only relevant for variable handling
	paramDummyBlock := components.NewParamDummyBlock(params)

	for _, paramField := range f.Type.Params.List {
		for _, param := range paramField.Names {
			switch fieldType := paramField.Type.(type) {
			case *ast.Ident, *ast.ArrayType:
				vi, err := g.parseVariableExpression(fieldType)
				if err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				vi.Name_ = param.Name

				np, err := paramDummyBlock.NewScopeVariable(vi)
				if err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				params[param.Name] = np

			case *ast.FuncType:
				vi, err := g.parseVariableExpression(fieldType)
				if err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				vi.Name_ = param.Name

				np, err := paramDummyBlock.NewScopeVariable(vi)
				if err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				params[param.Name] = np

				if err := paramDummyBlock.AddFunctionInterface(np); err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				g.defs.ScopeProperties[np.Name()] = &ScopeProperty{
					paramSize:  np.FuncIntf().Parameters.GetSize(),
					returnSize: np.FuncIntf().Results.GetSize(),
				}
			default:
				return nil, g.peb.NewParseError(f, errors.New("Type '"+reflect.TypeOf(fieldType).String()+"' is not supported in function param list"))
			}
		}
	}

	// Get return variables
	returnVars := []*variable.VariableInfo{}
	for _, paramField := range f.Type.Results.List {
		var vi *variable.VariableInfo

		switch fieldType := paramField.Type.(type) {
		case *ast.Ident, *ast.ArrayType:
			vic, err := g.parseVariableExpression(fieldType)
			if err != nil {
				return nil, g.peb.NewParseError(f, err)
			}

			vi, err = variable.FromDef(vic)
			if err != nil {
				return nil, g.peb.NewParseError(f, err)
			}

			infoPrinter.DebugPrintfln("Got result variable %s type %s size %d", vi.Name(), vi.Typ(), vi.Size_)
		case *ast.FuncType:
			return nil, g.peb.NewParseError(f, errors.New("FuncType is not supported in function results list"))
		default:
			return nil, g.peb.NewParseError(f, errors.New("Type '"+reflect.TypeOf(fieldType).String()+"' is not supported in function results list"))
		}

		returnVars = append(returnVars, vi)

		for i, param := range paramField.Names {
			if i != 0 {
				viCopy := vi.Copy()
				vi.Name_ = param.Name

				returnVars = append(returnVars, viCopy)
			} else {
				vi.Name_ = param.Name
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

	block, err := g.GenerateBlock(f.Body.List[0:fields-1], true, false, paramDummyBlock, true)
	if err != nil {
		return nil, g.peb.NewParseError(f, err)
	}

	rs, ok := f.Body.List[fields-1].(*ast.ReturnStmt)
	if !ok {
		return nil, g.peb.NewParseError(f, errors.New("Missing return statement at the end"))
	}

	if len(rs.Results) != len(returnVars) {
		return nil, g.peb.NewParseError(f, errors.New("Func results list length ("+strconv.Itoa(len(rs.Results))+") mismatches number of return vars ("+strconv.Itoa(len(returnVars))+")"))
	}

	// Overwrite name in return variables: this is to find the corresponding variable and maybe also case sizes
	// TODO: disallow implicit casting?

	for i, res := range rs.Results {
		switch x := res.(type) {
		case *ast.Ident:
			vi, err := block.GetVariable(x.Name)
			if err != nil {
				return nil, g.peb.NewParseError(f, err)
			}

			returnVars[i].Name_ = vi.Name()

			if vi.Size_ != returnVars[i].Size_ {
				infoPrinter.DebugPrintfln("@@@@@@@ Return variable size mismatch! %d != %d", vi.Size_, returnVars[i].Size_)
			}
			infoPrinter.DebugPrintfln("Overwriting name of return variable %s to %s typ %s and %s", returnVars[i].Name(), vi.Name(), returnVars[i].Typ(), vi.Typ())

			// TODO: check types?
		case *ast.IndexExpr:
			vi, err := block.GetVariable(x.X.(*ast.Ident).Name)
			if err != nil {
				return nil, g.peb.NewParseError(f, err)
			}

			switch indexType := x.Index.(type) {
			case *ast.BasicLit:
				vi.Index_ = indexType.Value
			case *ast.Ident:
				return nil, g.peb.NewParseError(f, errors.New("No identifier in index in returns allowed"))
			default:
				return nil, g.peb.NewParseError(f, errors.New("Invalid indexType in return"))
			}

			returnVars[i].Name_ = vi.Name()
			// returnVars = append(returnVars, vi)
			// TODO: check types?
		default:
			return nil, g.peb.NewParseError(f, errors.New("Invalid expression in return"))
		}
	}

	err = block.SetOutput(returnVars)
	if err != nil {
		return nil, g.peb.NewParseError(f, err)
	}

	s = components.NewScope(f.Name.Name, block /*, params, returnVars*/)

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

	// fetch function prototypes first

	infoPrinter.DebugPrintf("Parsing functions\n")

	for _, decl := range f.Decls {
		switch x := decl.(type) {
		case *ast.FuncDecl:
			vi, err := g.parseVariableExpression(x.Type)
			if err != nil {
				return g.peb.NewParseError(decl, err)
			}

			vi.Name_ = x.Name.Name

			g.functions[vi.Name_] = vi

			infoPrinter.DebugPrintf("Added function %s paramSize %d resultSize %d\n", vi.Name_, vi.FuncIntf_.Parameters.Size, vi.FuncIntf_.Results.Size)
		}
	}

	infoPrinter.DebugPrintf("Parsing functions done\n")

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

			infoPrinter.DebugPrintf("Adding entity '%s'\n", c.EntityName())

			// prefix entity if not already added
			ret = c.Entity() + ret
		}

		ret += c.Architecture()
	}

	for sn, s := range g.scopes {
		if _, ok := entityTracker[s.EntityName()]; !ok {
			entityTracker[s.EntityName()] = true

			infoPrinter.DebugPrintf("Adding entity '%s'\n", s.EntityName())

			// prefix entity if not already added
			ret = s.Entity() + ret
		}

		ps, rs := 0, 0
		for _, s := range s.Params {
			ps += s.Size_ * s.Len_
		}

		for _, v := range s.Params {
			fmt.Println(sn, ": Param ", v.String())
		}

		for i, s := range s.ReturnVars {
			fmt.Println(sn, ": ReturnVar(", i, ") ", s.String())

			rs += s.Size_ * s.Len_
		}

		g.defs.ScopeProperties[sn] = &ScopeProperty{
			paramSize:  s.Block.InputVariables().Size,
			returnSize: s.Block.OutputVariables().Size,
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
