package infoPrinter

import (
	"fmt"
	"go2async/internal/globalArguments"
)

func VerbosePrintf(formatString string, a ...interface{}) (n int, err error) {
	if *globalArguments.Verbose {
		return fmt.Printf("[VERBOSE]: "+formatString, a...)
	}
	return 0, nil
}

func VerbosePrintfln(formatString string, a ...interface{}) (n int, err error) {
	return VerbosePrintf("[VERBOSE]: "+formatString+"\n", a)
}

func VerbosePrintln(a ...interface{}) (n int, err error) {
	if *globalArguments.Verbose {
		return fmt.Println(append([]interface{}{"[VERBOSE]: "}, a...)...)
	}
	return 0, nil
}

func VerbosePrint(a ...interface{}) (n int, err error) {
	if *globalArguments.Verbose {
		return fmt.Print(append([]interface{}{"[VERBOSE]: "}, a...)...)
	}
	return 0, nil
}
