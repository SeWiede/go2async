package hwgenerator

import (
	"go/ast"
	"go/parser"
	"go/token"
	"testing"
)

const src = `
package circuit

func GCD(a , b int) int {
	for b != 0 {
		t := b
		//b = a % b
		c := a
		for c >= b {
			c = c - b
		}
		b = c
		a = t
	}
	return a
}
`

const srcLoop = `
package circuit

func TEST(a , b int) int {
	for a > 0 {
		if 3 > 2 {
			a = a - 1
			b = b + 1
		}
	}
}
`
const srcIf = `
package circuit

func TEST(a , b int) int {
	if a > 3 {
		a = a - 3
	}

	if b < 3 {
		b = b + 3
	}
}
`

const srcTEST = `
package goexamples

func TEST(a [5]int) int {
	var s, t [16]int
	return t
}

`

func TestCode(t *testing.T) {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "", srcTEST, 0)
	if err != nil {
		panic(err)
	}

	if len(f.Imports) > 0 {
		panic("Imports not allowed")
	}

	if len(f.Scope.Objects) > 1 {
		panic("Only one scope allowed")
	}

	ast.Print(fset, f)

	ast.Inspect(f.Decls[0], func(n ast.Node) bool {
		switch /* x :=  */ n.(type) {
		case *ast.GenDecl:
			return true
		case *ast.ImportSpec:
			return true
		case *ast.BasicLit:
			return true
		case *ast.Ident:
			return true
		case *ast.FuncDecl:
			/*
				gen := hwgenerator.NewGenerator()

				_, err := gen.GenerateScope(x)
				if err != nil {
					fmt.Println("Error: ", err)
					t.Fail()
				}

				fmt.Println(gen.GenerateVHDL()) */

			return false
		default:
			return true
		}
	})

	t.Fail()
}
