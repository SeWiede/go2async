package main

import (
	"fmt"
	"go2async/pkg/hwgenerator"
	"os"

	"github.com/spf13/cobra"
)

var Verbose bool
var IntSize *int
var Debug *bool

func newGo2AsyncCommand() (*cobra.Command, error) {
	cmd := &cobra.Command{
		Use:                   "go2async",
		Short:                 "go 2 async",
		SilenceErrors:         true,
		PersistentPreRun:      nil,
		DisableFlagsInUseLine: true,
	}

	// does nothing for now
	Debug = cmd.PersistentFlags().Bool("debug", false, "Enables debug mode")

	return cmd, nil
}

func main() {
	cmd, _ := newGo2AsyncCommand()

	genCmd := &cobra.Command{
		Use:     "generate <go file> [outputfile]",
		Aliases: []string{"gen", "g"},
		Short:   "Generate async vhdl from go code",
		RunE:    generate,
		Args:    cobra.RangeArgs(1, 2),
	}

	genCmd.Flags().BoolVar(&Verbose, "verbose", false, "Print informationa about internal signals")
	IntSize = genCmd.Flags().Int("intSize", 4, "overrides default intSize")
	cmd.AddCommand(genCmd)

	if _, err := cmd.ExecuteC(); err != nil {
		fmt.Fprintln(os.Stderr, "An error occured: ", err.Error())
		os.Exit(1)
	}
}

func generate(c *cobra.Command, args []string) error {
	file := args[0]
	outfile := os.Stdout
	if len(args) > 1 {
		of, err := os.OpenFile(args[1], os.O_CREATE|os.O_RDWR, 0644)
		if err != nil {
			fmt.Println("Failed to open file '"+args[1]+"': ", err.Error())
			return nil
		}
		outfile = of
	}

	gen := hwgenerator.NewGenerator(*IntSize)
	if err := gen.ParseGoFile(file); err != nil {
		fmt.Println("Parsing go file failed: ", err.Error())
		return nil
	}

	if err := gen.SaveVHDL(outfile, Verbose); err != nil {
		fmt.Println("Saving vhdl failed: ", err.Error())
		return nil
	}

	if outfile != os.Stdout {
		fmt.Println("Saved in ", outfile.Name())
	}

	return nil
}
