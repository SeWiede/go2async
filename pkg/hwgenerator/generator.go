package hwgenerator

import (
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go2async/internal/components"
	"go2async/internal/variable"
	"io/ioutil"
	"reflect"
	"strconv"
)

var ErrNotImplemented = errors.New("Not implemented")
var ErrTypeNotSupported = errors.New("Type is not supported")

var SupportedTypes map[string]int = map[string]int{"int": 4, "int8": 8, "int16": 16, "int32": 32, "int64": 64, "uint": 4, "uint8": 8, "uint16": 16, "uint32": 32, "uint64": 64, "byte": 8}

type Generator struct {
	wires          int
	knownVariables map[string]*variable.VariableInfo
	components     map[string]components.Component
	scopes         map[string]*components.Scope
	defs           *Defs
}

// current variable pos
var cvp = 0

func NewGenerator() *Generator {
	return &Generator{
		wires:          0,
		knownVariables: make(map[string]*variable.VariableInfo),
		components:     make(map[string]components.Component),
		scopes:         make(map[string]*components.Scope),
		defs:           NewDefs(4),
	}
}

// NewVariable sets new varible at next pos, does nothing if variable is already known; returns error on unknwon type
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
		}
		cvp += size
	}
	return g.knownVariables[name], nil
}

func (g *Generator) GetNewVarialbePos(name string) (*variable.VariableInfo, error) {
	if kv, ok := g.knownVariables[name]; !ok {
		return nil, errors.New("Unknown variable '" + name + "'")
	} else {
		return kv, nil
	}
}

func (g *Generator) GetNextWire() int {
	g.wires++
	return g.wires
}

func (g *Generator) GenerateFuncBlock(result *variable.VariableInfo, be *ast.BinaryExpr) (fb *components.FuncBlock, err error) {
	x := be.X
	y := be.Y

	xpos, xsize, ypos, ysize := 0, 0, 0, 0

	xconstval := ""
	yconstval := ""

	operation := be.Op.String()
	if _, ok := components.SupportedOperations[operation]; !ok {
		return nil, errors.New("Operation " + operation + " not supported")
	}

	switch t := x.(type) {
	case *ast.BasicLit:
		xconstval = t.Value
	case *ast.Ident:
		xvar, err := g.GetNewVarialbePos(t.Name)
		if err != nil {
			return nil, err
		}
		xpos = xvar.Position
		xsize = xvar.Size
	default:
		return nil, errors.New("Invalid type in binary expression: " + reflect.TypeOf(t).String())
	}

	switch t := y.(type) {
	case *ast.BasicLit:
		yconstval = t.Value
		ysize = xsize
	case *ast.Ident:
		yvar, err := g.GetNewVarialbePos(t.Name)
		if err != nil {
			return nil, err
		}
		ypos = yvar.Position
		ysize = yvar.Size
	default:
		return nil, errors.New("Invalid type in binary expression: " + reflect.TypeOf(t).String())
	}

	if xconstval != "" {
		xsize = ysize
	}

	return components.NewFuncBlock(operation, &components.OperandInfo{
		X_POS:       xpos,
		Y_POS:       ypos,
		RESULT_POS:  result.Position,
		X_SIZE:      xsize,
		Y_SIZE:      ysize,
		RESULT_SIZE: result.Size,
		XConstVal:   xconstval,
		YConstVal:   yconstval,
	}), nil //xpos, ypos, result_pos, xconstval, yconstval), nil
}

func (g *Generator) GenerateSelectorBlock(be *ast.BinaryExpr, inverted bool) (c *components.SelectorBlock, err error) {
	x := be.X
	y := be.Y

	xpos := 0
	xsize := 0
	ypos := 0
	ysize := 0

	xconstval := ""
	yconstval := ""

	switch t := x.(type) {
	case *ast.BasicLit:
		xconstval = t.Value
	case *ast.Ident:
		vi, err := g.GetNewVarialbePos(t.Name)
		if err != nil {
			return nil, err
		}
		xpos = vi.Position
		xsize = vi.Size
	default:
		return nil, errors.New("Invalid type in binary expression: " + reflect.TypeOf(t).String())
	}

	switch t := y.(type) {
	case *ast.BasicLit:
		yconstval = t.Value
		ysize = xsize
	case *ast.Ident:
		vi, err := g.GetNewVarialbePos(t.Name)
		if err != nil {
			return nil, err
		}
		ypos = vi.Position
		ysize = vi.Size
	default:
		return nil, errors.New("Invalid type in binary expression: " + reflect.TypeOf(t).String())
	}

	comp := be.Op.String()
	if _, ok := components.SupportedComperators[comp]; !ok {
		return nil, errors.New("Invalid comperator " + comp)
	}

	if xconstval != "" {
		xsize = ysize
	}

	return components.NewSelectorBlock(comp, &components.OperandInfo{
		X_POS:     xpos,
		Y_POS:     ypos,
		X_SIZE:    xsize,
		Y_SIZE:    ysize,
		XConstVal: xconstval,
		YConstVal: yconstval,
	}, inverted), nil
}

func (g *Generator) GenerateIfBlock(is *ast.IfStmt) (fb *components.IfBlock, err error) {
	var cond *components.SelectorBlock

	switch x := is.Cond.(type) {
	case *ast.BinaryExpr:
		cond, err = g.GenerateSelectorBlock(x, false)
		if err != nil {
			return nil, err
		}
	default:
		return nil, errors.New("Only binary expression condition allowed")
	}

	thenbody, err := g.GenerateBodyBlock(is.Body)
	if err != nil {
		return nil, err
	}

	var elsebody components.BodyComponent
	elsebody = components.NewFuncBlock("NOP", &components.OperandInfo{0, -1, 0, 0, 0, 0, "", ""}) //0, -1, 0, "", "")
	if is.Else != nil {
		elsebody, err = g.GenerateBodyBlock(is.Else)
		if err != nil {
			return nil, err
		}
	}

	g.components[cond.ArchName()] = cond
	g.components[thenbody.ArchName()] = thenbody
	g.components[elsebody.ArchName()] = elsebody

	return components.NewIfBlock(cond, thenbody, elsebody), nil
}

func (g *Generator) GenerateLoopBlock(fs *ast.ForStmt) (fb *components.LoopBlock, err error) {
	var cond *components.SelectorBlock

	switch x := fs.Cond.(type) {
	case *ast.BinaryExpr:
		cond, err = g.GenerateSelectorBlock(x, true)
		if err != nil {
			return nil, err
		}
	default:
		return nil, errors.New("Only binary expression condition allowed")
	}

	body, err := g.GenerateBodyBlock(fs.Body)
	if err != nil {
		return nil, err
	}

	g.components[cond.ArchName()] = cond
	g.components[body.ArchName()] = body

	return components.NewLoopBlock(cond, body), nil
}

func getTypeOfString(x string) string {
	if _, err := strconv.Atoi(x); err == nil {
		return "int"
	}

	return "string"
}

func getNodeLineString(n ast.Node) string {
	return strconv.Itoa(int(n.Pos()))
}

func (g *Generator) GenerateBodyBlock(s ast.Stmt) (c components.BodyComponent, err error) {
	switch x := s.(type) {
	case *ast.AssignStmt:
		if len(x.Lhs) > 1 {
			return nil, errors.New("Cannot assign more than one value")
		}
		var lhs *variable.VariableInfo
		newVar := false
		if x.Tok.String() == ":=" {
			newVar = true
		} else {
			lhs, err = g.GetNewVarialbePos(x.Lhs[0].(*ast.Ident).Name)
			if err != nil {
				return nil, err
			}

		}

		switch rhs := x.Rhs[0].(type) {
		case *ast.BasicLit:
			if newVar {
				typ := getTypeOfString(rhs.Value)
				if _, ok := SupportedTypes[typ]; !ok {
					return nil, errors.New(strconv.Itoa(int(x.Pos())) + ": Unsupported type '" + typ + "'")
				}
				lhs, err = g.NewVariable(x.Lhs[0].(*ast.Ident).Name, typ)
				if err != nil {
					return nil, err
				}
			}

			return components.NewFuncBlock("NOP", &components.OperandInfo{-1, -1, lhs.Position, -1, -1, lhs.Size, rhs.Value, ""}), nil
		case *ast.Ident:
			v, err := g.GetNewVarialbePos(rhs.Name)
			if err != nil {
				return nil, err
			}

			if newVar {
				lhs, err = g.NewVariable(x.Lhs[0].(*ast.Ident).Name, v.Typ)
				if err != nil {
					return nil, err
				}
			}

			return components.NewFuncBlock("NOP", &components.OperandInfo{v.Position, -1, lhs.Position, v.Size, 0, lhs.Size, "", ""}), nil //g.GetNewVarialbePos(rhs.Name), -1, lhs, "", ""), nil
		case *ast.BinaryExpr:
			if newVar {
				return nil, errors.New(getNodeLineString(x) + ": Cannot initialize variable with binary expression")
			}
			return g.GenerateFuncBlock(lhs, rhs)
		default:
			return nil, errors.New("Expression " + reflect.TypeOf(x).String() + " not supported!")
		}
	case *ast.ForStmt:
		return g.GenerateLoopBlock(x)
	case *ast.IfStmt:
		return g.GenerateIfBlock(x)
	case *ast.ReturnStmt:
		return nil, errors.New("Return statements only allowed at the end of function!")
	case *ast.BlockStmt:
		return g.GenerateBlock(x.List, false)
	default:
		return nil, errors.New(reflect.TypeOf(x).String() + " statements not allowed!")
	}
}

func (g *Generator) GenerateBlock(stmts []ast.Stmt, toplevelStatement bool) (c *components.Block, err error) {
	b := components.NewBlock(toplevelStatement)
	for _, s := range stmts {
		newComponent, err := g.GenerateBodyBlock(s)
		if err != nil {
			return nil, err
		}

		g.components[newComponent.ArchName()] = newComponent
		b.AddComponent(newComponent)
	}
	return b, nil
}

func (g *Generator) GenerateScope(f *ast.FuncDecl) (s *components.Scope, err error) {
	params := make(map[string]*variable.VariableInfo)
	for _, field := range f.Type.Params.List {
		typeField, ok := field.Type.(*ast.Ident)
		if !ok {
			return nil, errors.New("Invalid field type in function param list")
		}

		for _, param := range field.Names {
			np, err := g.NewVariable(param.Name, typeField.Name)
			if err != nil {
				return nil, err
			}
			params[param.Name] = np
		}
	}

	fields := len(f.Body.List)
	if fields < 2 {
		return nil, errors.New("At least one top-level statement + return expected")
	}
	block, err := g.GenerateBlock(f.Body.List[0:fields-1], true)
	if err != nil {
		return nil, err
	}

	rs, ok := f.Body.List[fields-1].(*ast.ReturnStmt)
	if !ok {
		return nil, errors.New("Missing return statement at the end")
	}

	returnPositions := make(map[string]*variable.VariableInfo)
	for _, res := range rs.Results {
		switch x := res.(type) {
		case *ast.Ident:
			vi, ok := g.knownVariables[x.Name]
			if !ok {
				return nil, errors.New("No new variable in returns allowed")
			}
			returnPositions[x.Name] = vi
		default:
			return nil, errors.New("Only identifiers returns allowed")
		}
	}

	s = components.NewScope(f.Name.Name, block, params, g.knownVariables, returnPositions)

	g.components[block.ArchName()] = block
	g.scopes[s.ArchName()] = s
	return s, nil
}

func (g *Generator) ParseGoFile(file string) error {
	fset := token.NewFileSet()

	b, _ := ioutil.ReadFile(file)

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

func (g *Generator) GenerateVHDL() string {
	ret := ""

	for _, c := range g.components {
		ret += c.Architecture()
	}

	svs, ps, rs := 0, 0, 0
	for _, s := range g.knownVariables {
		svs += s.Size
	}

	for sn, s := range g.scopes {
		for _, s := range s.Params {
			ps += s.Size
		}
		for _, s := range s.ReturnVars {
			rs += s.Size
		}

		g.defs.ScopeProperties[sn] = &ScopeProperty{
			sumVarSize: svs,
			paramSize:  ps,
			returnSize: rs,
		}
		ret += s.Architecture()
	}

	fmt.Println(len(g.knownVariables), " Variables found")
	for k, v := range g.knownVariables {
		fmt.Println("Var ", k, " is at index ", v)
	}

	return g.defs.GetDefs() + ret
}

func (g *Generator) SaveVHDL(file string) error {
	err := ioutil.WriteFile(file, []byte(g.GenerateVHDL()), 0644)
	if err != nil {
		return err
	}
	return nil
}
