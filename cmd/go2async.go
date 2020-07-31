package main

import (
	"fmt"
	"go2async/pkg/hwgenerator"
	"os"

	"github.com/spf13/cobra"
)

var printVhd bool

func newGo2AsyncCommand() (*cobra.Command, error) {
	cmd := &cobra.Command{
		Use:                   "go2async",
		Short:                 "go 2 async",
		SilenceErrors:         true,
		SilenceUsage:          true,
		PersistentPreRun:      nil,
		DisableFlagsInUseLine: true,
	}

	// does nothing for now
	cmd.PersistentFlags().Bool("debug", false, "Enables debug mode")

	return cmd, nil
}

func main() {
	cmd, _ := newGo2AsyncCommand()

	genCmd := &cobra.Command{
		Use:     "generate <go file>",
		Aliases: []string{"gen", "g"},
		Short:   "Generate async vhdl from go code",
		RunE:    generate,
		Args:    cobra.RangeArgs(1, 2),
	}
	genCmd.Flags().BoolVar(&printVhd, "s", false, "Print generated vhd")
	cmd.AddCommand(genCmd)

	if _, err := cmd.ExecuteC(); err != nil {
		fmt.Fprint(os.Stderr, "An error occured: ", err.Error())
		os.Exit(1)
	}
}

func generate(c *cobra.Command, args []string) error {
	file := args[0]
	outfile := "../vhd/go2async.vhd"
	if len(args) > 1 {
		outfile = args[1]
	}

	gen := hwgenerator.NewGenerator()
	if err := gen.ParseGoFile(file); err != nil {
		return err
	}

	if err := gen.SaveVHDL(outfile); err != nil {
		return err
	}

	if printVhd {
		fmt.Println(gen.GenerateVHDL())
	}

	return nil
}
