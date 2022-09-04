package hwgenerator

import (
	"fmt"
)

const defsTemplate = "\tconstant %s_OUT_DATA_WIDTH : Integer := %d;\n\tconstant %s_IN_DATA_WIDTH : Integer := %d;"

type ScopeProperty struct {
	paramSize  int
	returnSize int
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
		ret += fmt.Sprintf(defsTemplate,
			scope, prop.returnSize,
			scope, prop.paramSize)
		ret += "\n\n"
	}

	ret += "end package;\n"
	return ret
}
