package main

import (
	"fmt"
	"os"
	"os/exec"
	"slices"
	"strings"
)

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Usage: klar <command> <file> [<output file>]")
		fmt.Println("\nAvailable commands:")
		fmt.Println("  tokenize    : Tokenize and print the tokens")
		fmt.Println("  parse       : Parse and print the AST")
		fmt.Println("  typecheck   : Verify the types and the overall correctness of the program")
		fmt.Println("  generate-ir : Generate the intermediate representation (IR)")
		fmt.Println("  generate-asm: Generate Darwin ARM64 assembly")
		fmt.Println("  build       : Compile, link, and write the binary to <output file>")
		fmt.Println("  run         : Build and run")
		os.Exit(1)
	}
	cmd := os.Args[1]
	if !slices.Contains([]string{"tokenize", "parse", "typecheck", "generate-ir", "generate-asm", "build", "run"}, cmd) {
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
		fmt.Println("Failed to read file: ", err)
		os.Exit(1)
	}
	tokens, err := Tokenize(src, file)
	if err != nil {
		fmt.Println("Failed to tokenize: ", err)
		os.Exit(1)
	}
	if cmd == "tokenize" {
		for _, token := range tokens {
			fmt.Println(token)
		}
		os.Exit(0)
	}
	node, err := Parse(tokens)
	if err != nil {
		fmt.Println("Failed to parse: ", err)
		os.Exit(1)
	}
	if cmd == "parse" {
		fmt.Println(node)
		os.Exit(0)
	}
	ty, typeMap, err := TypeCheck(node)
	if err != nil {
		fmt.Println("Failed to typecheck: ", err)
		os.Exit(1)
	}
	if cmd == "typecheck" {
		fmt.Println(ty)
		fmt.Println(typeMap)
		os.Exit(0)
	}
	instructions, err := GenerateIR(node, typeMap)
	if err != nil {
		fmt.Println("Failed to generate the IR: ", err)
		os.Exit(1)
	}
	if cmd == "generate-ir" {
		for _, instruction := range instructions {
			fmt.Println(instruction)
		}
		os.Exit(0)
	}
	asm, err := GenerateDarwinArm64ASM(instructions)
	if err != nil {
		fmt.Println("Failed to generate assembly: ", err)
		os.Exit(1)
	}
	if cmd == "generate-asm" {
		fmt.Println(asm.String())
		os.Exit(0)
	}
	var targetFile string
	if cmd == "build" {
		targetFile = os.Args[3]
	} else {
		tmpFile, err := os.CreateTemp("", "klar_run_*")
		if err != nil {
			fmt.Println(fmt.Errorf("Failed to create temporary file: %v", err))
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
