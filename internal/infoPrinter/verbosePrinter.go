package infoprinter

import (
	"fmt"
	"go2async/internal/globalArguments"
)

func VerbosePrintf(formatString string, a ...interface{}) (n int, err error) {
	if *globalArguments.Verbose {
		return fmt.Printf(formatString, a...)
	}
	return 0, nil
}

func VerbosePrintln(a ...interface{}) (n int, err error) {
	if *globalArguments.Verbose {
		return fmt.Println(a...)
	}
	return 0, nil
}

func VerbosePrint(a ...interface{}) (n int, err error) {
	if *globalArguments.Verbose {
		return fmt.Print(a...)
	}
	return 0, nil
}
