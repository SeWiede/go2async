package infoPrinter

import (
	"fmt"
	"go2async/internal/globalArguments"
)

func DebugPrintf(formatString string, a ...interface{}) (n int, err error) {
	if *globalArguments.Debug {
		return fmt.Printf(formatString, a...)
	}
	return 0, nil
}

func DebugPrintfln(formatString string, a ...interface{}) (n int, err error) {
	return DebugPrintf(formatString+"\n", a)
}

func DebugPrintln(a ...interface{}) (n int, err error) {
	if *globalArguments.Debug {
		return fmt.Println(a...)
	}
	return 0, nil
}

func DebugPrint(a ...interface{}) (n int, err error) {
	if *globalArguments.Debug {
		return fmt.Print(a...)
	}
	return 0, nil
}
