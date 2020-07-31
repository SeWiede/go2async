package hwgenerator

import (
	"fmt"
	"strconv"
)

const defsTemplate = `constant %s_VARIABLE_WIDTH : Integer := %d;
	constant %s_DATA_MULTIPLIER : Integer := %d;
	constant %s_DATA_WIDTH : Integer := %s_VARIABLE_WIDTH * %s_DATA_MULTIPLIER;
	constant %s_OUT_DATA_WIDTH : Integer := %s_VARIABLE_WIDTH * %d;
	constant %s_IN_DATA_WIDTH : Integer := %s_VARIABLE_WIDTH * %d;
	`

type ScopeProperty struct {
	varCount    int
	varWidth    int
	returnCount int
	paramCount  int
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
		constant VARIABLE_WIDTH: Integer := ` + strconv.Itoa(d.DefaulVariableWidth) + `;
	`

	for scope, prop := range d.ScopeProperties {
		ret += fmt.Sprintf(defsTemplate, scope, prop.varWidth,
			scope, prop.varCount,
			scope, scope, scope,
			scope, scope, prop.returnCount,
			scope, scope, prop.paramCount)
		ret += "\n"
	}

	ret += "end package;\n"
	return ret
}
