package main

import (
	"fmt"
	"os"
	"slices"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/codegen"
	"github.com/flunderpero/klar/bootstrap/ir"
	"github.com/flunderpero/klar/bootstrap/lower"
	"github.com/flunderpero/klar/bootstrap/token"
	"github.com/flunderpero/klar/bootstrap/typed"
)

func usage() {
	fmt.Println("Usage: klar <command> [options]")
	fmt.Println("\nAvailable commands:")
	fmt.Println("  tokens <file>             : Tokenize and print the tokens")
	fmt.Println("  ast <file>                : Parse and print the AST")
	fmt.Println("  types <file>              : Verify the types and the overall correctness of the program")
	fmt.Println("  lower <file>              : Lower and simplify the AST")
	fmt.Println("  ir <file>                 : Generate the intermediate representation (IR)")
	fmt.Println("  asm <file>                : Generate Darwin ARM64 assembly")
	fmt.Println("  build <file> <outfile>    : Compile, link, and write the binary to <output file>")
	fmt.Println("  run <file>                : Build and run")
	fmt.Println("  examples [--update] [file]: Build and run examples")
	os.Exit(1)
}

func main() {
	cmd := os.Args[1]
	if !slices.Contains([]string{"tokens", "ast", "types", "lower", "ir", "asm", "build", "run", "examples"}, cmd) {
		fmt.Println("Unknown command: ", cmd)
		usage()
		os.Exit(1)
	}
	if cmd == "examples" {
		update := false
		file := ""
		if len(os.Args) > 2 {
			update = os.Args[2] == "--update"
			if !update {
				file = os.Args[2]
			} else if len(os.Args) > 3 {
				file = os.Args[3]
			}
		}
		if file != "" {
			if err := runTestFile(file, update); err != nil {
				os.Exit(1)
			}
		} else if err := runTestFiles("./examples", update); err != nil {
			os.Exit(1)
		}
		os.Exit(0)
	}
	if len(os.Args) < 3 {
		usage()
	}
	var targetFile string
	if cmd == "build" {
		if len(os.Args) < 4 {
			fmt.Println("Usage: klar build <file> <output file>")
			os.Exit(1)
		}
		targetFile = os.Args[3]
	}
	if cmd == "run" {
		tmpFile, err := os.CreateTemp("", "klar_run_*")
		if err != nil {
			fmt.Printf("Failed to create temporary file: %+v\n", err)
			os.Exit(1)
		}
		defer os.Remove(tmpFile.Name())
		targetFile = tmpFile.Name()
	}
	file := os.Args[2]
	src, err := os.ReadFile(file)
	if err != nil {
		fmt.Printf("Failed to read file: %+v\n", err)
		os.Exit(1)
	}
	var astModule *ast.Module
	compiler := Compiler{
		OnTokenize: func(tokens []token.Token) bool {
			if cmd == "tokens" {
				for i, token := range tokens {
					if i > 0 {
						fmt.Print(" ")
					}
					fmt.Print(token)
				}
				fmt.Println()
				return false
			}
			return true
		},
		OnParse: func(module *ast.Module) bool {
			astModule = module
			if cmd == "ast" {
				fmt.Println(module)
				return false
			}
			return true
		},
		OnTypeCheck: func(typeInfo *typed.TypeInfo) bool {
			if cmd == "types" {
				printTypedAST(astModule, typeInfo)
				return false
			}
			return true
		},
		OnLowered: func(ast *lower.LoweredAST) bool {
			if cmd == "lower" {
				fmt.Println(ast)
				return false
			}
			return true
		},
		OnIR: func(module *ir.Module) bool {
			if cmd == "ir" {
				fmt.Println(module)
				return false
			}
			return true
		},
		OnASM: func(code *codegen.ASMText) bool {
			if cmd == "asm" {
				fmt.Println(code)
				return false
			}
			return true
		},
	}
	unit := CompilationUnit{src, file}
	if cmd == "run" {
		if runCmd, err := compiler.CompileAndRun(unit, os.Stdout, os.Stderr); err != nil {
			fmt.Println("Failed:", err)
			os.Exit(runCmd.ProcessState.ExitCode())
		}
	} else {
		if err := compiler.Compile(unit, targetFile); err != nil {
			fmt.Println("Failed:", err)
			os.Exit(1)
		}
		if cmd == "build" {
			fmt.Println("Done.")
			os.Exit(0)
		}
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
	suffix := ""
	if expr, ok := node.(*ast.IdentExpression); ok {
		if _, ok := v.typeInfo.LookupTypeBinding(expr); ok {
			suffix = " !typebinding"
		}
	}
	fmt.Printf("%s\n=> %s (#%s)%s\n\n", node, ty, ty.Id(), suffix)
	return nil
}
