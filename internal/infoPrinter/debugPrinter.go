package infoPrinter

import (
	"fmt"
	"go2async/internal/globalArguments"
)

var debugCnt = -1

func debugPrefix() string {
	debugCnt++
	return fmt.Sprintf("[%6d   DEBUG]: ", debugCnt)
}

func DebugPrintf(formatString string, a ...interface{}) (n int, err error) {
	if *globalArguments.Debug {
		return fmt.Printf(debugPrefix()+formatString, a...)
	}
	return 0, nil
}

func DebugPrintfln(formatString string, a ...interface{}) (n int, err error) {
	return DebugPrintf(formatString+"\n", a...)
}

func DebugPrintln(a ...interface{}) (n int, err error) {
	if *globalArguments.Debug {
		return fmt.Println(append([]interface{}{debugPrefix()}, a...)...)
	}
	return 0, nil
}

func DebugPrint(a ...interface{}) (n int, err error) {
	if *globalArguments.Debug {
		return fmt.Print(append([]interface{}{debugPrefix()}, a...)...)
	}
	return 0, nil
}
