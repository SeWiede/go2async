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

var ErrNotImplemented = errors.New("Not implemented")
var ErrTypeNotSupported = errors.New("Type is not supported")

var SupportedTypes map[string]int = map[string]int{"int": strconv.IntSize, "int8": 8, "int16": 16, "int32": 32, "int64": 64, "uint": strconv.IntSize, "uint8": 8, "uint16": 16, "uint32": 32, "uint64": 64, "byte": 8}

type Generator struct {
	wires          int
	knownVariables map[string]*variable.VariableInfo
	components     map[string]components.Component
	scopes         map[string]*components.Scope
	defs           *Defs

	peb *parseErrorBuilder
}

// current variable pos
var cvp = 0

func NewGenerator(intSize int) *Generator {
	SupportedTypes["int"] = intSize
	SupportedTypes["uint"] = intSize

	components.SupportedTypes["int"] = intSize
	components.SupportedTypes["uint"] = intSize

	return &Generator{
		wires:          0,
		knownVariables: make(map[string]*variable.VariableInfo),
		components:     make(map[string]components.Component),
		scopes:         make(map[string]*components.Scope),
		defs:           NewDefs(intSize),
	}
}

func (g *Generator) GenerateFuncBlock(result *variable.VariableInfo, be *ast.BinaryExpr, parent *components.Block) (fb *components.FuncBlock, err error) {
	xexpr := be.X
	yexpr := be.Y

	var x, y *variable.VariableInfo

	operation := be.Op.String()
	if _, ok := components.SupportedOperations[operation]; !ok {
		return nil, g.peb.NewParseError(be, errors.New("Operation "+operation+" not supported"))
	}

	switch t := xexpr.(type) {
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

	return components.NewFuncBlock(operation, &components.OperandInfo{
		R: result,
		X: x,
		Y: y,
	}, parent), nil
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

	thenBody, err := g.GenerateBodyBlock(is.Body, parent)
	if err != nil {
		return nil, g.peb.NewParseError(is, err)
	}

	var elseBody components.BodyComponentType
	elseBody = components.NewFuncBlock("NOP", &components.OperandInfo{
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
		elseBody, err = g.GenerateBlock(is.Else.(*ast.BlockStmt).List, false, parent)
		if err != nil {
			return nil, g.peb.NewParseError(is, err)
		}
	}

	g.components[cond.ArchName()] = cond
	g.components[thenBody.ArchName()] = thenBody
	g.components[elseBody.ArchName()] = elseBody

	return components.NewIfBlock(cond, thenBody, elseBody, parent), nil
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

	body, err := g.GenerateBlock(fs.Body.List, false, parent)
	if err != nil {
		return nil, g.peb.NewParseError(fs, err)
	}

	g.components[cond.ArchName()] = cond
	g.components[body.ArchName()] = body

	return components.NewLoopBlock(cond, body, parent), nil
}

func getTypeOfString(x string) string {
	if _, err := strconv.Atoi(x); err == nil {
		return "uint8"
	}

	return "string"
}

func getNodeLineString(n ast.Node) string {
	return strconv.Itoa(int(n.Pos()))
}

func (g *Generator) GenerateBodyBlock(s ast.Stmt, parent *components.Block) (c components.BodyComponentType, err error) {
	switch lhsexpr := s.(type) {
	case *ast.DeclStmt:
		decl, ok := lhsexpr.Decl.(*ast.GenDecl)
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
		if len(lhsexpr.Lhs) > 1 {
			return nil, g.peb.NewParseError(s, errors.New("Cannot assign more than one value"))
		}
		var lhs *variable.VariableInfo
		newVar := false
		if lhsexpr.Tok.String() == ":=" {
			newVar = true
		} else {

			switch lhstype := lhsexpr.Lhs[0].(type) {
			case *ast.Ident:
				lhs, err = parent.GetVariable(lhstype.Name)
				if err != nil {
					return nil, g.peb.NewParseError(s, err)
				}
			case *ast.IndexExpr:
				X, ok := lhstype.X.(*ast.Ident)
				if !ok {
					return nil, g.peb.NewParseError(s, errors.New("Lhs index expression X has to be an identifier"))
				}

				lhs, err = parent.GetVariable(X.Name)
				if err != nil {
					return nil, g.peb.NewParseError(s, err)
				}

				switch indexNode := lhstype.Index.(type) {
				case *ast.BasicLit:
					if indexNode.Kind != token.INT {
						return nil, g.peb.NewParseError(s, errors.New("Invalid indexing value kind '"+indexNode.Kind.String()+"'"))
					}
					lhs.Index = indexNode.Value
				case *ast.Ident:
					v, err := parent.GetVariable(indexNode.Name)
					if err != nil {
						return nil, g.peb.NewParseError(s, err)
					}

					lhs.IndexIdent = v
				default:
					return nil, g.peb.NewParseError(s, errors.New("Invalid indexing"))
				}
			default:
				return nil, g.peb.NewParseError(s, errors.New("Invalid lhs type '"+reflect.TypeOf(lhsexpr).String()+"'"))
			}
		}

		switch rhsExpr := lhsexpr.Rhs[0].(type) {
		case *ast.BasicLit:
			if newVar {
				typ := getTypeOfString(rhsExpr.Value)
				if _, ok := SupportedTypes[typ]; !ok {
					return nil, g.peb.NewParseError(s, errors.New(strconv.Itoa(int(lhsexpr.Pos()))+": Unsupported type '"+typ+"'"))
				}

				if lhs, err = parent.NewVariable(lhsexpr.Lhs[0].(*ast.Ident).Name, typ, 1); err != nil {
					return nil, g.peb.NewParseError(s, err)
				}
			}

			return components.NewFuncBlock("=", &components.OperandInfo{
				R: lhs,
				X: &variable.VariableInfo{
					Const: rhsExpr.Value,
					Size:  lhs.Size,
				},
			}, parent), nil
		case *ast.Ident:
			v, err := parent.GetVariable(rhsExpr.Name)
			if err != nil {
				return nil, g.peb.NewParseError(s, err)
			}

			if newVar {
				if lhs, err = parent.NewVariable(lhsexpr.Lhs[0].(*ast.Ident).Name, v.Typ, 1); err != nil {
					return nil, g.peb.NewParseError(s, err)
				}
			}

			return components.NewFuncBlock("=", &components.OperandInfo{
				R: lhs,
				X: v,
			}, parent), nil
		case *ast.BinaryExpr:
			if newVar {
				return nil, g.peb.NewParseError(s, errors.New(getNodeLineString(lhsexpr)+": Cannot initialize variable with binary expression"))
			}
			return g.GenerateFuncBlock(lhs.Copy(), rhsExpr, parent)
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
				if lhs, err = parent.NewVariable(lhsexpr.Lhs[0].(*ast.Ident).Name, v.Typ, 1); err != nil {
					return nil, g.peb.NewParseError(s, err)
				}
			}
			return components.NewFuncBlock("=", &components.OperandInfo{
				R: lhs,
				X: v,
			}, parent), nil
		default:
			return nil, g.peb.NewParseError(s, errors.New("Expression "+reflect.TypeOf(rhsExpr).String()+" in rhs not supported!"))
		}
	case *ast.ForStmt:
		return g.GenerateLoopBlock(lhsexpr, parent)
	case *ast.IfStmt:
		return g.GenerateIfBlock(lhsexpr, parent)
	case *ast.ReturnStmt:
		return nil, g.peb.NewParseError(s, errors.New("Return statements only allowed at the end of function!"))
	case *ast.BlockStmt:
		return g.GenerateBlock(lhsexpr.List, false, parent)
	default:
		return nil, g.peb.NewParseError(s, errors.New("Invalid statement "+reflect.TypeOf(lhsexpr).String()))
	}
}

func (g *Generator) GenerateBlock(stmts []ast.Stmt, toplevelStatement bool, parent *components.Block) (b *components.Block, err error) {
	b = components.NewBlock(toplevelStatement, parent)

	for _, s := range stmts {
		newComponent, err := g.GenerateBodyBlock(s, b)
		if err != nil {
			return nil, g.peb.NewParseError(s, err)
		}
		if newComponent == nil {
			continue
		}

		g.components[newComponent.ArchName()] = newComponent
		b.AddComponent(newComponent)
	}

	return b, nil
}

func (g *Generator) GenerateScope(f *ast.FuncDecl) (s *components.Scope, err error) {
	params := make(map[string]*variable.VariableInfo)

	// only relevant for variable handling
	paramDummyBlock := components.NewParamDummyBlock(params)

	for _, field := range f.Type.Params.List {
		for _, param := range field.Names {
			switch fieldType := field.Type.(type) {
			case *ast.Ident:
				np, err := paramDummyBlock.NewVariable(param.Name, fieldType.Name, 1)
				if err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				params[param.Name] = np
			case *ast.ArrayType:
				len, err := strconv.Atoi(fieldType.Len.(*ast.BasicLit).Value)
				if err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				np, err := paramDummyBlock.NewVariable(param.Name, fieldType.Elt.(*ast.Ident).Name, len)
				if err != nil {
					return nil, g.peb.NewParseError(f, err)
				}

				params[param.Name] = np
			default:
				return nil, g.peb.NewParseError(f, errors.New("Invalid field type in function param list"))
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

	// TODO: assign toplevelstatement bool true again later
	block, err := g.GenerateBlock(f.Body.List[0:fields-1], false, paramDummyBlock)
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
		}
	}

	s = components.NewScope(f.Name.Name, block, params, g.knownVariables, returnPositions)

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
		return errors.New("No imports allowed")
	}

	if len(f.Scope.Objects) > 1 {
		return errors.New("Only one scope allowed")
	}

	//ast.Print(fset, f)
	switch x := f.Decls[0].(type) {
	case *ast.FuncDecl:
		_, err := g.GenerateScope(x)
		if err != nil {
			return err
		}
	default:
		return errors.New(reflect.TypeOf(x).String() + " not allowed! Expected fucntion declaration")
	}

	return nil
}

func (g *Generator) GenerateVHDL() string {
	ret := ""

	for _, c := range g.components {
		ret += c.Architecture()
	}

	svs, ps, rs := 0, 0, 0
	for _, s := range g.knownVariables {
		svs += s.Size * s.Len
	}

	for sn, s := range g.scopes {
		for _, s := range s.Params {
			ps += s.Size * s.Len
		}

		for i, s := range s.ReturnVars {
			infoprinter.VerbosePrintf("return %d type %s has size %d len %d thus %d\n", i, s.Typ, s.Size, s.Len, s.Size*s.Len)
			rs += s.Size * s.Len
		}

		g.defs.ScopeProperties[sn] = &ScopeProperty{
			sumVarSize: svs,
			paramSize:  s.Block.GetVariableSize(),
			returnSize: rs,
		}
		ret += s.Architecture()

		for k, v := range s.Params {
			lB := strconv.Itoa(v.Position)
			uB := strconv.Itoa(v.Position + v.Size)
			if v.Len > 1 {
				k = k + "[k]"
				lB += " + " + strconv.Itoa(v.Size) + " * k"
				uB = strconv.Itoa(v.Position) + " + " + strconv.Itoa(v.Size) + " * (k + 1)"
			}
			fmt.Println(sn, ": Param ", k, " is at (", uB, " - 1 downto ", lB, ") ")
		}
	}

	if *globalArguments.Verbose {
		for k, v := range g.knownVariables {
			lB := strconv.Itoa(v.Position)
			uB := strconv.Itoa(v.Position + v.Size)
			if v.Len > 1 {
				k = k + "[k]"
				lB += " + " + strconv.Itoa(v.Size) + " * k"
				uB = strconv.Itoa(v.Position) + " + " + strconv.Itoa(v.Size) + " * (k + 1)"
			}
			fmt.Println(k, " is at (", uB, " - 1 downto ", lB, ") ")
		}
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
