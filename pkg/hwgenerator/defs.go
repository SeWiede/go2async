package hwgenerator

import (
	"fmt"
)

const defsTemplate = `constant %s_DATA_WIDTH : Integer := %d;
	constant %s_OUT_DATA_WIDTH : Integer := %d;
	constant %s_IN_DATA_WIDTH : Integer := %d;
	`

type ScopeProperty struct {
	sumVarSize int
	returnSize int
	paramSize  int
}

type Defs struct {
	ScopeProperties     map[string]*ScopeProperty
	DefaulVariableWidth int
}

func NewDefs(defaulVariableWidth int) *Defs {
	return &Defs{
		ScopeProperties:     make(map[string]*ScopeProperty),
		DefaulVariableWidth: defaulVariableWidth,
	}
}

func (d *Defs) GetDefs() string {
	ret := `library IEEE;
	use IEEE.std_logic_1164.all;
	
	package defs is
	`

	for scope, prop := range d.ScopeProperties {
		ret += fmt.Sprintf(defsTemplate, scope, prop.sumVarSize,
			scope, prop.returnSize,
			scope, prop.paramSize)
		ret += "\n"
	}

	ret += "end package;\n"
	return ret
}
