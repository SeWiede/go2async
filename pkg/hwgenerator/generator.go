package hwgenerator

import (
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go2async/internal/components"
	"io/ioutil"
	"reflect"
)

var ErrNotImplemented = errors.New("Not implemented")
var ErrTypeNotSupported = errors.New("Type is not supported")

type Generator struct {
	wires      int
	variables  map[string]int
	components map[string]components.Component
	scopes     map[string]*components.Scope
	defs       *Defs
}

// current variable pos
var cvp = 0

func NewGenerator() *Generator {
	return &Generator{
		wires:      0,
		variables:  make(map[string]int),
		components: make(map[string]components.Component),
		scopes:     make(map[string]*components.Scope),
		defs:       NewDefs(4),
	}
}

// NewVariable sets new varible at given pos, does nothing if variable is already known
func (g *Generator) NewVariable(name string) {
	if _, ok := g.variables[name]; !ok {
		g.variables[name] = cvp
		cvp++
	}
}

func (g *Generator) GetNewVarialbePos(name string) int {
	if _, ok := g.variables[name]; !ok {
		g.NewVariable(name)
	}
	return g.variables[name]
}

func (g *Generator) GetNextWire() int {
	g.wires++
	return g.wires
}

func (g *Generator) GenerateFuncBlock(result_pos int, be *ast.BinaryExpr) (fb *components.FuncBlock, err error) {
	x := be.X
	y := be.Y

	xpos := 0
	ypos := 0

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
		xpos = g.GetNewVarialbePos(t.Name)
	default:
		return nil, errors.New("Invalid type in binary expression: " + reflect.TypeOf(t).String())
	}

	switch t := y.(type) {
	case *ast.BasicLit:
		yconstval = t.Value
	case *ast.Ident:
		ypos = g.GetNewVarialbePos(t.Name)
	default:
		return nil, errors.New("Invalid type in binary expression: " + reflect.TypeOf(t).String())
	}

	return components.NewFuncBlock(operation, xpos, ypos, result_pos, xconstval, yconstval), nil
}

func (g *Generator) GenerateSelectorBlock(be *ast.BinaryExpr, inverted bool) (c *components.SelectorBlock, err error) {
	x := be.X
	y := be.Y

	xpos := 0
	ypos := 0

	xconstval := ""
	yconstval := ""

	switch t := x.(type) {
	case *ast.BasicLit:
		xconstval = t.Value
	case *ast.Ident:
		xpos = g.GetNewVarialbePos(t.Name)
	default:
		return nil, errors.New("Invalid type in binary expression: " + reflect.TypeOf(t).String())
	}

	switch t := y.(type) {
	case *ast.BasicLit:
		yconstval = t.Value
	case *ast.Ident:
		ypos = g.GetNewVarialbePos(t.Name)
	default:
		return nil, errors.New("Invalid type in binary expression: " + reflect.TypeOf(t).String())
	}

	return components.NewSelectorBlock(be.Op.String(), xpos, ypos, xconstval, yconstval, inverted), nil
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
	elsebody = components.NewFuncBlock("", 0, -1, 0, "", "")
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

func (g *Generator) GenerateBodyBlock(s ast.Stmt) (c components.BodyComponent, err error) {
	switch x := s.(type) {
	case *ast.AssignStmt:
		lhs := g.GetNewVarialbePos(x.Lhs[0].(*ast.Ident).Name)
		switch rhs := x.Rhs[0].(type) {
		case *ast.BasicLit:
			return components.NewFuncBlock("", -1, -1, lhs, rhs.Value, ""), nil
		case *ast.Ident:
			return components.NewFuncBlock("", g.GetNewVarialbePos(rhs.Name), -1, lhs, "", ""), nil
		case *ast.BinaryExpr:
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
	paramCount := 0
	for _, field := range f.Type.Params.List {
		if t, ok := field.Type.(*ast.Ident); !ok || t.Name != "int" {
			return nil, errors.New("Invalid type in function param list")
		}
		for _, param := range field.Names {
			paramCount++
			g.GetNewVarialbePos(param.Name)
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

	returnPositions := []int{}
	for _, res := range rs.Results {
		switch x := res.(type) {
		case *ast.Ident:
			pos, ok := g.variables[x.Name]
			if !ok {
				return nil, errors.New("No new variable in returns allowed")
			}
			returnPositions = append(returnPositions, pos)
		default:
			return nil, errors.New("Only identifiers returns allowed")
		}
	}

	s = components.NewScope(f.Name.Name, block, 4, len(g.variables), paramCount, returnPositions)

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

	for sn, s := range g.scopes {
		g.defs.ScopeProperties[sn] = &ScopeProperty{
			returnCount: len(s.ReturnPositions),
			varCount:    s.VarCount,
			varWidth:    s.VarWidth,
			paramCount:  s.ParamCount,
		}
		ret += s.Architecture()
	}

	fmt.Println(len(g.variables), " Variables found")
	for k, v := range g.variables {
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
