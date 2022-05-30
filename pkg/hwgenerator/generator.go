package hwgenerator

import (
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go2async/internal/components"
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

/* // NewVariable sets new varible at next pos, does nothing if variable is already known; returns error on unknwon type
func (g *Generator) NewVariable(name string, typ string) (*variable.VariableInfo, error) {
	size, ok := SupportedTypes[typ]
	if !ok {
		return nil, errors.New("Unsupported type '" + typ + "'")
	}
	if _, ok := g.knownVariables[name]; !ok {
		g.knownVariables[name] = &variable.VariableInfo{
			Position: cvp,
			Size:     size,
			Typ:      typ,
			Len:      1,
		}
		cvp += size
	}
	return g.knownVariables[name].Copy(), nil
}

// NewVariable sets new varible at next pos, does nothing if variable is already known; returns error on unknwon type
func (g *Generator) NewArrayVariable(name string, typ string, lenStr string) (*variable.VariableInfo, error) {
	size, ok := SupportedTypes[typ]
	if !ok {
		return nil, errors.New("Unsupported type '" + typ + "'")
	}
	len, err := strconv.Atoi(lenStr)
	if err != nil {
		return nil, err
	}
	if len <= 0 {
		return nil, errors.New("Invalid array lentgh " + lenStr)
	}

	if _, ok := g.knownVariables[name]; !ok {
		g.knownVariables[name] = &variable.VariableInfo{
			Position: cvp,
			Size:     size,
			Typ:      typ,
			Len:      len,
		}
		cvp += size * len
	}
	return g.knownVariables[name].Copy(), nil
}

func (g *Generator) GetVariablePos(name string) (*variable.VariableInfo, error) {
	if kv, ok := g.knownVariables[name]; !ok {
		return nil, errors.New("Unknown variable '" + name + "'")
	} else {
		return kv.Copy(), nil
	}
} */

func (g *Generator) GetNextWire() int {
	g.wires++
	return g.wires
}

func (g *Generator) GenerateFuncBlock(result *variable.VariableInfo, be *ast.BinaryExpr, predecessor components.BodyComponent) (fb *components.FuncBlock, err error) {
	xexpr := be.X
	yexpr := be.Y

	var x, y *variable.VariableInfo

	operation := be.Op.String()
	if _, ok := components.SupportedOperations[operation]; !ok {
		return nil, errors.New("Operation " + operation + " not supported")
	}

	switch t := xexpr.(type) {
	case *ast.BasicLit:
		x = &variable.VariableInfo{
			Const: t.Value,
			Size:  result.Size,
		}
	case *ast.Ident:
		/* x, err = g.GetVariablePos(t.Name)
		if err != nil {
			return nil, err
		} */

		x = components.GetVariable(predecessor, t.Name)
		if x == nil {
			return nil, errors.New("variableInfo is nil")
		}
	case *ast.IndexExpr:
		/* x, err = g.GetVariablePos(t.X.(*ast.Ident).Name)
		if err != nil {
			return nil, err
		} */

		x = components.GetVariable(predecessor, t.X.(*ast.Ident).Name)
		if x == nil {
			return nil, errors.New("variableInfo is nil")
		}

		switch indexNode := t.Index.(type) {
		case *ast.BasicLit:
			if indexNode.Kind != token.INT {
				return nil, errors.New("Invalid indexing value kind '" + indexNode.Kind.String() + "'")
			}
			x.Index = indexNode.Value
		case *ast.Ident:
			/* v, err := g.GetVariablePos(indexNode.Name)
			if err != nil {
				return nil, err
			} */

			v := components.GetVariable(predecessor, t.X.(*ast.Ident).Name)
			if v == nil {
				return nil, errors.New("variableInfo is nil")
			}

			x.IndexIdent = v
		default:
			return nil, errors.New("Invalid indexing")
		}
	default:
		ref := reflect.TypeOf(t)
		if ref == nil {
			return nil, errors.New("Invalid type in binary expression: <nil>")
		}
		return nil, errors.New("Invalid type in binary expression: " + ref.String())
	}

	switch t := yexpr.(type) {
	case *ast.BasicLit:
		y = &variable.VariableInfo{
			Const: t.Value,
			Size:  result.Size,
		}
	case *ast.Ident:
		/* y, err = g.GetVariablePos(t.Name)
		if err != nil {
			return nil, err
		} */

		y = components.GetVariable(predecessor, t.Name)
		if y == nil {
			return nil, errors.New("variableInfo is nil")
		}
	case *ast.IndexExpr:
		/* y, err = g.GetVariablePos(t.X.(*ast.Ident).Name)
		if err != nil {
			return nil, err
		} */

		y = components.GetVariable(predecessor, t.X.(*ast.Ident).Name)
		if y == nil {
			return nil, errors.New("variableInfo is nil")
		}

		switch indexNode := t.Index.(type) {
		case *ast.BasicLit:
			if indexNode.Kind != token.INT {
				return nil, errors.New("Invalid indexing value kind '" + indexNode.Kind.String() + "'")
			}
			y.Index = indexNode.Value
		case *ast.Ident:
			/* v, err := g.GetVariablePos(indexNode.Name)
			if err != nil {
				return nil, err
			} */

			v := components.GetVariable(predecessor, indexNode.Name)
			if v == nil {
				return nil, errors.New("variableInfo is nil")
			}

			y.IndexIdent = v
		default:
			return nil, errors.New("Invalid indexing")
		}
	default:
		ref := reflect.TypeOf(t)
		if ref == nil {
			return nil, errors.New("Invalid type in binary expression: <nil>")
		}
		return nil, errors.New("Invalid type in binary expression: " + ref.String())
	}

	return components.NewFuncBlock(operation, &components.OperandInfo{
		R: result,
		X: x,
		Y: y,
	}, predecessor), nil
}

func (g *Generator) GenerateSelectorBlock(be *ast.BinaryExpr, inverted bool, predecessor components.BodyComponent) (c *components.SelectorBlock, err error) {
	xexpr := be.X
	yexpr := be.Y

	var x, y *variable.VariableInfo

	comp := be.Op.String()
	if _, ok := components.SupportedComperators[comp]; !ok {
		return nil, errors.New("Invalid comperator " + comp)
	}

	switch t := xexpr.(type) {
	case *ast.BasicLit:
		x = &variable.VariableInfo{
			Const: t.Value,
		}
	case *ast.Ident:
		/* x, err = g.GetVariablePos(t.Name)
		if err != nil {
			return nil, err
		} */

		x = components.GetVariable(predecessor, t.Name)
		if x == nil {
			return nil, errors.New("variableInfo is nil")
		}
	case *ast.IndexExpr:
		/* x, err = g.GetVariablePos(t.X.(*ast.Ident).Name)
		if err != nil {
			return nil, err
		} */

		x = components.GetVariable(predecessor, t.X.(*ast.Ident).Name)
		if x == nil {
			return nil, errors.New("variableInfo is nil")
		}

		switch indexNode := t.Index.(type) {
		case *ast.BasicLit:
			if indexNode.Kind != token.INT {
				return nil, errors.New("Invalid indexing value kind '" + indexNode.Kind.String() + "'")
			}
			x.Index = indexNode.Value
		case *ast.Ident:
			/* v, err := g.GetVariablePos(indexNode.Name)
			if err != nil {
				return nil, err
			} */

			v := components.GetVariable(predecessor, indexNode.Name)
			if v == nil {
				return nil, errors.New("variableInfo is nil")
			}

			x.IndexIdent = v
		default:
			return nil, errors.New("Invalid indexing")
		}
	default:
		ref := reflect.TypeOf(t)
		if ref == nil {
			return nil, errors.New("Invalid type in binary expression: <nil>")
		}
		return nil, errors.New("Invalid type in binary expression: " + ref.String())
	}

	switch t := yexpr.(type) {
	case *ast.BasicLit:
		y = &variable.VariableInfo{
			Const: t.Value,
			Size:  x.Size,
		}
	case *ast.Ident:
		/* y, err = g.GetVariablePos(t.Name)
		if err != nil {
			return nil, err
		} */

		y = components.GetVariable(predecessor, t.Name)
		if y == nil {
			return nil, errors.New("variableInfo is nil")
		}
	case *ast.IndexExpr:
		/* y, err = g.GetVariablePos(t.X.(*ast.Ident).Name)
		if err != nil {
			return nil, err
		} */

		y = components.GetVariable(predecessor, t.X.(*ast.Ident).Name)
		if y == nil {
			return nil, errors.New("variableInfo is nil")
		}

		switch indexNode := t.Index.(type) {
		case *ast.BasicLit:
			if indexNode.Kind != token.INT {
				return nil, errors.New("Invalid indexing value kind '" + indexNode.Kind.String() + "'")
			}
			y.Index = indexNode.Value
		case *ast.Ident:
			/* v, err := g.GetVariablePos(indexNode.Name)
			if err != nil {
				return nil, err
			} */

			v := components.GetVariable(predecessor, indexNode.Name)
			if v == nil {
				return nil, errors.New("variableInfo is nil")
			}

			y.IndexIdent = v
		default:
			return nil, errors.New("Invalid indexing")
		}
	default:
		ref := reflect.TypeOf(t)
		if ref == nil {
			return nil, errors.New("Invalid type in binary expression: <nil>")
		}
		return nil, errors.New("Invalid type in binary expression: " + ref.String())
	}

	if x.Const != "" {
		x.Size = y.Size
	}

	return components.NewSelectorBlock(comp, &components.OperandInfo{
		X: x,
		Y: y,
	}, inverted, predecessor), nil
}

func (g *Generator) GenerateIfBlock(is *ast.IfStmt, predecessor components.BodyComponent) (fb *components.IfBlock, err error) {
	var cond *components.SelectorBlock

	switch x := is.Cond.(type) {
	case *ast.BinaryExpr:
		cond, err = g.GenerateSelectorBlock(x, false, predecessor)
		if err != nil {
			return nil, err
		}
	case *ast.ParenExpr:
		xBin, ok := x.X.(*ast.BinaryExpr)
		if !ok {
			ref := reflect.TypeOf(x.X)
			if ref == nil {
				return nil, errors.New("Only binary expression in if condition allowed found <nil> in parenthesis")
			}
			return nil, errors.New("Only binary expression in if condition allowed found " + ref.String() + " in parenthesis")
		}
		cond, err = g.GenerateSelectorBlock(xBin, false, predecessor)
		if err != nil {
			return nil, err
		}
	default:
		ref := reflect.TypeOf(x)
		if ref == nil {
			return nil, errors.New("Only binary expression in if condition allowed found <nil>")
		}
		return nil, errors.New("Only binary expression in if condition allowed found " + ref.String())
	}

	thenbody, err := g.GenerateBodyBlock(is.Body, predecessor)
	if err != nil {
		return nil, err
	}

	var elsebody components.BodyComponent
	elsebody = components.NewFuncBlock("NOP", &components.OperandInfo{
		R: &variable.VariableInfo{
			Position: 0,
			Size:     8,
		},
		X: &variable.VariableInfo{
			Position: 0,
			Size:     8,
		},
	}, predecessor)
	if is.Else != nil {
		elsebody, err = g.GenerateBodyBlock(is.Else, predecessor)
		if err != nil {
			return nil, err
		}
	}

	g.components[cond.ArchName()] = cond
	g.components[thenbody.ArchName()] = thenbody
	g.components[elsebody.ArchName()] = elsebody

	return components.NewIfBlock(cond, thenbody, elsebody, predecessor), nil
}

func (g *Generator) GenerateLoopBlock(fs *ast.ForStmt, predecessor components.BodyComponent) (fb *components.LoopBlock, err error) {
	var cond *components.SelectorBlock

	switch x := fs.Cond.(type) {
	case *ast.BinaryExpr:
		cond, err = g.GenerateSelectorBlock(x, true, predecessor)
		if err != nil {
			return nil, err
		}
	case *ast.ParenExpr:
		xBin, ok := x.X.(*ast.BinaryExpr)
		if !ok {
			ref := reflect.TypeOf(x.X)
			if ref == nil {
				return nil, errors.New("Only binary expression in for condition allowed found <nil> in parenthesis")
			}
			return nil, errors.New("Only binary expression in for condition allowed found " + reflect.TypeOf(x.X).String() + " in parenthesis")
		}
		cond, err = g.GenerateSelectorBlock(xBin, false, predecessor)
		if err != nil {
			return nil, err
		}
	default:
		ref := reflect.TypeOf(x)
		if ref == nil {
			return nil, errors.New("Only binary expression in for condition allowed found <nil>")
		}
		return nil, errors.New("Only binary expression in for condition allowed found " + reflect.TypeOf(x).String())
	}

	fmt.Printf("Generating body for loop\n")

	body, err := g.GenerateBodyBlock(fs.Body, predecessor)
	if err != nil {
		return nil, err
	}

	g.components[cond.ArchName()] = cond
	g.components[body.ArchName()] = body

	variableDummyBlock := components.NewParamDummyBlock(predecessor.ScopedVariables(), predecessor)
	*variableDummyBlock.GetVariablesSize() = *predecessor.GetVariablesSize()

	return components.NewLoopBlock(cond, body, variableDummyBlock), nil
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

func (g *Generator) GenerateBodyBlock(s ast.Stmt, predecessor components.BodyComponent) (c components.BodyComponent, err error) {
	switch lhsexpr := s.(type) {
	case *ast.DeclStmt:
		decl, ok := lhsexpr.Decl.(*ast.GenDecl)
		if !ok {
			return nil, errors.New("Invalid declaration type!")
		} else {
			if decl.Tok != token.VAR {
				return nil, errors.New("Only var declaration allowed!")
			}
		}

		for _, spec := range decl.Specs[0].(*ast.ValueSpec).Names {
			switch declType := decl.Specs[0].(*ast.ValueSpec).Type.(type) {
			case *ast.Ident:
				/* _, err = g.NewVariable(spec.Name, declType.Name)
				if err != nil {
					return nil, err
				} */

				if _, err := components.NewVariable(predecessor, spec.Name, declType.Name, 1); err != nil {
					return nil, err
				}
			case *ast.ArrayType:
				len, ok := declType.Len.(*ast.BasicLit)
				if !ok {
					return nil, errors.New("Index in array declaration as to be a basic lit of kind INT")
				} else {
					if len.Kind != token.INT {
						return nil, errors.New("Index in array declaration as to be a basic lit of kind INT")
					}
				}
				elt, ok := declType.Elt.(*ast.Ident)
				if !ok {
					return nil, errors.New("Invalid array declaration")
				}

				/* _, err = g.NewArrayVariable(spec.Name, elt.Name, len.Value)
				if err != nil {
					return nil, err
				} */

				leni, err := strconv.Atoi(len.Value)
				if err != nil {
					return nil, err
				}

				if _, err := components.NewVariable(predecessor, spec.Name, elt.Name, leni); err != nil {
					return nil, err
				}
			default:
				return nil, errors.New("Invalid declaration")
			}
		}

		return nil, nil
	case *ast.AssignStmt:
		if len(lhsexpr.Lhs) > 1 {
			return nil, errors.New("Cannot assign more than one value")
		}
		var lhs *variable.VariableInfo
		newVar := false
		if lhsexpr.Tok.String() == ":=" {
			newVar = true
		} else {

			switch lhstype := lhsexpr.Lhs[0].(type) {
			case *ast.Ident:
				/* lhs, err = g.GetVariablePos(lhstype.Name)
				if err != nil {
					return nil, err
				} */

				lhs = components.GetVariable(predecessor, lhstype.Name)
				if lhs == nil {
					return nil, errors.New("variableInfo is nil")
				}
			case *ast.IndexExpr:
				X, ok := lhstype.X.(*ast.Ident)
				if !ok {
					return nil, errors.New("Lhs index expression X has to be an identifier")
				}

				/* lhs, err = g.GetVariablePos(X.Name)
				if err != nil {
					return nil, err
				} */
				lhs = components.GetVariable(predecessor, X.Name)
				if lhs == nil {
					return nil, errors.New("variableInfo is nil")
				}

				switch indexNode := lhstype.Index.(type) {
				case *ast.BasicLit:
					if indexNode.Kind != token.INT {
						return nil, errors.New("Invalid indexing value kind '" + indexNode.Kind.String() + "'")
					}
					lhs.Index = indexNode.Value
				case *ast.Ident:
					/* v, err := g.GetVariablePos(indexNode.Name)
					if err != nil {
						return nil, err
					} */

					v := components.GetVariable(predecessor, indexNode.Name)
					if v == nil {
						return nil, errors.New("variableInfo is nil")
					}

					lhs.IndexIdent = v
				default:
					return nil, errors.New("Invalid indexing")
				}
			default:
				return nil, errors.New("Invalid lhs type '" + reflect.TypeOf(lhsexpr).String() + "'")
			}
		}

		switch rhsExpr := lhsexpr.Rhs[0].(type) {
		case *ast.BasicLit:
			if newVar {
				typ := getTypeOfString(rhsExpr.Value)
				if _, ok := SupportedTypes[typ]; !ok {
					return nil, errors.New(strconv.Itoa(int(lhsexpr.Pos())) + ": Unsupported type '" + typ + "'")
				}

				/* lhs, err = g.NewVariable(lhsexpr.Lhs[0].(*ast.Ident).Name, typ)
				if err != nil {
					return nil, err
				} */

				if lhs, err = components.NewVariable(predecessor, lhsexpr.Lhs[0].(*ast.Ident).Name, typ, 1); err != nil {
					return nil, err
				}
			}

			return components.NewFuncBlock("=", &components.OperandInfo{
				R: lhs,
				X: &variable.VariableInfo{
					Const: rhsExpr.Value,
					Size:  lhs.Size,
				},
			}, predecessor), nil
		case *ast.Ident:
			/* v, err := g.GetVariablePos(rhsExpr.Name)
			if err != nil {
				return nil, err
			} */

			v := components.GetVariable(predecessor, rhsExpr.Name)
			if v == nil {
				return nil, errors.New("variableInfo is nil")
			}

			if newVar {
				/* lhs, err = g.NewVariable(lhsexpr.Lhs[0].(*ast.Ident).Name, v.Typ)
				if err != nil {
					return nil, err
				} */

				if lhs, err = components.NewVariable(predecessor, lhsexpr.Lhs[0].(*ast.Ident).Name, v.Typ, 1); err != nil {
					return nil, err
				}
			}

			return components.NewFuncBlock("=", &components.OperandInfo{
				R: lhs,
				X: v,
			}, predecessor), nil
		case *ast.BinaryExpr:
			if newVar {
				return nil, errors.New(getNodeLineString(lhsexpr) + ": Cannot initialize variable with binary expression")
			}
			return g.GenerateFuncBlock(lhs.Copy(), rhsExpr, predecessor)
		case *ast.IndexExpr:
			/* v, err := g.GetVariablePos(rhsExpr.X.(*ast.Ident).Name)
			if err != nil {
				return nil, err
			} */

			v := components.GetVariable(predecessor, rhsExpr.X.(*ast.Ident).Name)
			if v == nil {
				return nil, errors.New("variableInfo is nil")
			}

			switch indexNode := rhsExpr.Index.(type) {
			case *ast.BasicLit:
				if indexNode.Kind != token.INT {
					return nil, errors.New("Invalid indexing value kind '" + indexNode.Kind.String() + "'")
				}
				v.Index = indexNode.Value
			case *ast.Ident:
				/* vn, err := g.GetVariablePos(indexNode.Name)
				if err != nil {
					return nil, err
				} */

				vn := components.GetVariable(predecessor, indexNode.Name)
				if vn == nil {
					return nil, errors.New("variableInfo is nil")
				}

				v.IndexIdent = vn
			default:
				return nil, errors.New("Invalid indexing")
			}

			if newVar {
				/* lhs, err = g.NewVariable(lhsexpr.Lhs[0].(*ast.Ident).Name, v.Typ)
				if err != nil {
					return nil, err
				} */

				if lhs, err = components.NewVariable(predecessor, lhsexpr.Lhs[0].(*ast.Ident).Name, v.Typ, 1); err != nil {
					return nil, err
				}
			}
			return components.NewFuncBlock("=", &components.OperandInfo{
				R: lhs,
				X: v,
			}, predecessor), nil
		default:
			return nil, errors.New("Expression " + reflect.TypeOf(rhsExpr).String() + " in rhs not supported!")
		}
	case *ast.ForStmt:
		return g.GenerateLoopBlock(lhsexpr, predecessor)
	case *ast.IfStmt:
		return g.GenerateIfBlock(lhsexpr, predecessor)
	case *ast.ReturnStmt:
		return nil, errors.New("Return statements only allowed at the end of function!")
	case *ast.BlockStmt:
		r, _, err := g.GenerateBlock(lhsexpr.List, false, predecessor)
		return r, err
	default:
		return nil, errors.New(reflect.TypeOf(lhsexpr).String() + " statements not allowed!")
	}
}

func (g *Generator) GenerateBlock(stmts []ast.Stmt, toplevelStatement bool, predecessor components.BodyComponent) (b *components.Block, predecessor_ components.BodyComponent, err error) {
	b = components.NewBlock(toplevelStatement, predecessor)
	predecessor_ = b
	for _, s := range stmts {
		newComponent, err := g.GenerateBodyBlock(s, predecessor_)
		if err != nil {
			return nil, nil, err
		}
		if newComponent == nil {
			continue
		}

		g.components[newComponent.ArchName()] = newComponent
		b.AddComponent(newComponent)

		predecessor_ = newComponent
	}

	//*b.GetVariablesSize() = *predecessor_.GetVariablesSize()

	return b, predecessor_, nil
}

func (g *Generator) GenerateScope(f *ast.FuncDecl) (s *components.Scope, err error) {
	params := make(map[string]*variable.VariableInfo)

	// only relevant for variable handling
	paramDummyBlock := components.NewParamDummyBlock(params, nil)

	for _, field := range f.Type.Params.List {
		for _, param := range field.Names {
			switch fieldType := field.Type.(type) {
			case *ast.Ident:
				/* np, err := g.NewVariable(param.Name, fieldType.Name)
				if err != nil {
					return nil, err
				} */

				np, err := components.NewVariable(paramDummyBlock, param.Name, fieldType.Name, 1)
				if err != nil {
					return nil, err
				}

				params[param.Name] = np
			case *ast.ArrayType:
				/* np, err := g.NewArrayVariable(param.Name, fieldType.Elt.(*ast.Ident).Name, fieldType.Len.(*ast.BasicLit).Value)
				if err != nil {
					return nil, err
				} */

				len, err := strconv.Atoi(fieldType.Len.(*ast.BasicLit).Value)
				if err != nil {
					return nil, err
				}

				np, err := components.NewVariable(paramDummyBlock, param.Name, fieldType.Elt.(*ast.Ident).Name, len)
				if err != nil {
					return nil, err
				}

				params[param.Name] = np
			default:
				return nil, errors.New("Invalid field type in function param list")
			}
		}
	}

	if len(params) == 0 {
		return nil, errors.New("At least one function parameter expected")
	}

	fields := len(f.Body.List)
	if fields < 2 {
		return nil, errors.New("At least one top-level statement + return expected")
	}

	block, lastBodyComp, err := g.GenerateBlock(f.Body.List[0:fields-1], true, paramDummyBlock)
	if err != nil {
		return nil, err
	}
	block.OutputSize = *lastBodyComp.GetVariablesSize()

	// last block outputs all declared variables
	//*block.GetVariablesSize() = *lastBodyComp.GetVariablesSize()

	rs, ok := f.Body.List[fields-1].(*ast.ReturnStmt)
	if !ok {
		return nil, errors.New("Missing return statement at the end")
	}

	returnPositions := []*variable.VariableInfo{}
	for _, res := range rs.Results {
		switch x := res.(type) {
		case *ast.Ident:
			/* vi, ok := g.knownVariables[x.Name]
			if !ok {
				return nil, errors.New("No new variable in returns allowed")
			} */

			vi := components.GetVariable(lastBodyComp, x.Name)
			if vi == nil {
				return nil, errors.New("No new variable in returns allowed")
			}

			returnPositions = append(returnPositions, vi)
		case *ast.IndexExpr:
			/* vi, ok := g.knownVariables[x.X.(*ast.Ident).Name]
			if !ok {
				return nil, errors.New("Invalid return base address identifier")
			} */

			vi := components.GetVariable(lastBodyComp, x.X.(*ast.Ident).Name)
			if vi == nil {
				return nil, errors.New("No new variable in returns allowed")
			}

			switch indexType := x.Index.(type) {
			case *ast.BasicLit:
				vi.Index = indexType.Value
			case *ast.Ident:
				return nil, errors.New("No identifier in index in returns allowed")
			default:
				return nil, errors.New("Invalid indexType in return")
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

	b, err := ioutil.ReadFile(file)
	if err != nil {
		return err
	}

	f, err := parser.ParseFile(fset, "", string(b), 0)
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

func (g *Generator) GenerateVHDL(verbose bool) string {
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
			fmt.Printf("return %d type %s has size %d len %d thus %d\n", i, s.Typ, s.Size, s.Len, s.Size*s.Len)
			rs += s.Size * s.Len
		}

		g.defs.ScopeProperties[sn] = &ScopeProperty{
			sumVarSize: svs,
			paramSize:  ps,
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

	if verbose {
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

func (g *Generator) SaveVHDL(file *os.File, verbose bool) error {
	file.Truncate(0)
	_, err := file.Write([]byte(g.GenerateVHDL(verbose)))
	if err != nil {
		return err
	}
	return nil
}
