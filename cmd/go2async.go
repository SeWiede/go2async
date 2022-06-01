package main

import (
	"fmt"
	"go2async/internal/globalArguments"
	"go2async/pkg/hwgenerator"
	"os"

	"github.com/spf13/cobra"
)

func newGo2AsyncCommand() (*cobra.Command, error) {
	cmd := &cobra.Command{
		Use:                   "go2async",
		Short:                 "go 2 async",
		SilenceErrors:         true,
		PersistentPreRun:      nil,
		DisableFlagsInUseLine: true,
	}

	// does nothing for now
	globalArguments.Debug = cmd.PersistentFlags().Bool("debug", false, "Enables debug mode")
	globalArguments.Verbose = cmd.PersistentFlags().Bool("verbose", false, "Print additional information")

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

	globalArguments.IntSize = genCmd.Flags().Int("intSize", 4, "overrides default intSize")
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

	gen := hwgenerator.NewGenerator(*globalArguments.IntSize)
	if err := gen.ParseGoFile(file); err != nil {
		fmt.Println("Parsing go file failed:\n", err.Error())
		return nil
	}

	if err := gen.SaveVHDL(outfile); err != nil {
		fmt.Println("Saving vhdl failed: ", err.Error())
		return nil
	}

	if outfile != os.Stdout {
		fmt.Println("Saved in ", outfile.Name())
	}

	return nil
}
