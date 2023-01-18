package infoPrinter

import (
	"fmt"
	"go2async/internal/globalArguments"
)

var verboseCnt = -1

func verbosePrefix() string {
	verboseCnt++
	return fmt.Sprintf("[%6d VERBOSE]: ", verboseCnt)
}

func VerbosePrintf(formatString string, a ...interface{}) (n int, err error) {
	if *globalArguments.Verbose {
		return fmt.Printf(verbosePrefix()+formatString, a...)
	}
	return 0, nil
}

func VerbosePrintfln(formatString string, a ...interface{}) (n int, err error) {
	return VerbosePrintf(verbosePrefix()+formatString+"\n", a)
}

func VerbosePrintln(a ...interface{}) (n int, err error) {
	if *globalArguments.Verbose {
		return fmt.Println(append([]interface{}{verbosePrefix()}, a...)...)
	}
	return 0, nil
}

func VerbosePrint(a ...interface{}) (n int, err error) {
	if *globalArguments.Verbose {
		return fmt.Print(append([]interface{}{verbosePrefix()}, a...)...)
	}
	return 0, nil
}
