package main

import (
	"fmt"
	"os"
	"os/exec"
	"slices"
	"strings"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/codegen"
	"github.com/flunderpero/klar/bootstrap/ir"
	"github.com/flunderpero/klar/bootstrap/lower"
	"github.com/flunderpero/klar/bootstrap/token"
	"github.com/flunderpero/klar/bootstrap/typed"
)

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Usage: klar <command> <file> [<output file>]")
		fmt.Println("\nAvailable commands:")
		fmt.Println("  tokens : Tokenize and print the tokens")
		fmt.Println("  ast    : Parse and print the AST")
		fmt.Println("  types  : Verify the types and the overall correctness of the program")
		fmt.Println("  lower  : Lower and simplify the AST")
		fmt.Println("  ir     : Generate the intermediate representation (IR)")
		fmt.Println("  asm    : Generate Darwin ARM64 assembly")
		fmt.Println("  build  : Compile, link, and write the binary to <output file>")
		fmt.Println("  run    : Build and run")
		os.Exit(1)
	}
	cmd := os.Args[1]
	if !slices.Contains([]string{"tokens", "ast", "types", "lower", "ir", "asm", "build", "run"}, cmd) {
		fmt.Println("Unknown command: ", cmd)
		os.Exit(1)
	}
	if cmd == "build" && len(os.Args) < 4 {
		fmt.Println("Usage: klar build <file> <output file>")
		os.Exit(1)
	}
	file := os.Args[2]
	src, err := os.ReadFile(file)
	if err != nil {
		fmt.Printf("Failed to read file: %+v\n", err)
		os.Exit(1)
	}
	tokens, err := token.Tokenize(src, file)
	if err != nil {
		fmt.Printf("Failed to tokenize: %+v\n", err)
		os.Exit(1)
	}
	if cmd == "tokens" {
		for _, token := range tokens {
			fmt.Println(token)
		}
		os.Exit(0)
	}
	fileParts := strings.Split(strings.Split(file, ".")[0], "/")
	moduleName := fileParts[len(fileParts)-1]
	module, err := ast.Parse(tokens, ast.Ident(moduleName))
	if err != nil {
		fmt.Printf("Failed to parse: %+v\n", err)
		os.Exit(1)
	}
	if cmd == "ast" {
		fmt.Println(module)
		os.Exit(0)
	}
	ty, typeInfo, err := typed.TypeCheck(module)
	if err != nil {
		fmt.Printf("Failed to typecheck: %+v\n", err)
		os.Exit(1)
	}
	if cmd == "types" {
		fmt.Println(ty)
		printTypedAST(module, typeInfo)
		os.Exit(0)
	}
	lowered, err := lower.Lower(module, typeInfo)
	if err != nil {
		fmt.Printf("Failed to lower: %+v\n", err)
		os.Exit(1)
	}
	if cmd == "lower" {
		fmt.Println(lowered)
		os.Exit(0)
	}
	irModule, err := ir.GenerateIR(lowered, typeInfo)
	if err != nil {
		fmt.Printf("Failed to generate the IR: %+v\n", err)
		os.Exit(1)
	}
	if cmd == "ir" {
		for _, constant := range irModule.Constants {
			fmt.Println(constant.String())
		}
		for _, ty := range irModule.DeclaredTypes.Types {
			switch ty := ty.(type) {
			case ir.BuiltInType:
			default:
				fmt.Println("@declare", ty)
			}
		}
		fmt.Println()
		for _, function := range irModule.Functions {
			fmt.Println(function, "{")
			err := ir.WalkBlock(function.Entry, func(block *ir.Block) error {
				fmt.Println(block)
				return nil
			})
			if err != nil {
				fmt.Printf("Failed to print the IR: %+v\n", err)
				os.Exit(1)
			}
			fmt.Println("}")
			fmt.Println("RegisterConstraints:")
			fmt.Println(function.RegisterConstraints.String())
		}
		os.Exit(0)
	}
	asm, err := codegen.GenerateDarwinArm64ASM(irModule)
	if err != nil {
		fmt.Printf("Failed to generate assembly: %+v\n", err)
		os.Exit(1)
	}
	if cmd == "asm" {
		fmt.Println(asm.String())
		os.Exit(0)
	}
	var targetFile string
	if cmd == "build" {
		targetFile = os.Args[3]
	} else {
		tmpFile, err := os.CreateTemp("", "klar_run_*")
		if err != nil {
			fmt.Printf("Failed to create temporary file: %+v\n", err)
			os.Exit(1)
		}
		defer os.Remove(tmpFile.Name())
		targetFile = tmpFile.Name()
	}
	buildCmd := exec.Command("clang", "-o", targetFile, "-x", "assembler", "-")
	buildCmd.Stdin = strings.NewReader(asm.String())
	buildCmd.Stdout = os.Stdout
	buildCmd.Stderr = os.Stderr
	if err = buildCmd.Run(); err != nil {
		fmt.Println("Build failed.")
		os.Exit(buildCmd.ProcessState.ExitCode())
	}
	if cmd == "build" {
		fmt.Println("Done.")
		os.Exit(0)
	}
	runCmd := exec.Command(targetFile)
	runCmd.Stdout = os.Stdout
	runCmd.Stderr = os.Stderr
	if err = runCmd.Run(); err != nil {
		fmt.Println("Run failed")
		os.Exit(runCmd.ProcessState.ExitCode())
	}
}

func printTypedAST(node ast.Node, typeInfo *typed.TypeInfo) {
	visitor := &printTypedASTWalker{ast.DefaultVisitor{}, typeInfo}
	walker := &ast.DefaultWalker{Visitor: visitor}
	if err := walker.WalkNode(node); err != nil {
		fmt.Printf("ERROR: %+v\n", err)
	}
}

type printTypedASTWalker struct {
	ast.DefaultVisitor
	typeInfo *typed.TypeInfo
}

func (v *printTypedASTWalker) VisitNode(node ast.Node, w ast.Walker) error {
	if err := w.WalkNode(node); err != nil {
		return err
	}
	ty := v.typeInfo.MustLookup(node)
	fmt.Printf("%s\n=> %s\n\n", node, ty)
	return nil
}
