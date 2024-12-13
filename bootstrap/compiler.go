package main

import (
	"io"
	"os"
	"os/exec"
	"strings"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/codegen"
	"github.com/flunderpero/klar/bootstrap/ir"
	"github.com/flunderpero/klar/bootstrap/lower"
	"github.com/flunderpero/klar/bootstrap/token"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type Compiler struct {
	OnTokenize  func(tokens []token.Token) bool
	OnParse     func(module *ast.Module) bool
	OnTypeCheck func(typeInfo *typed.TypeInfo) bool
	OnLowered   func(ast *lower.LoweredAST) bool
	OnIR        func(module *ir.Module) bool
	OnASM       func(code *codegen.ASMText) bool
}

type CompilationUnit struct {
	src  []byte
	file string
}

func (self *Compiler) CompileAndRun(unit CompilationUnit, stdout io.Writer, stderr io.Writer) (*exec.Cmd, error) {
	tmpFile, err := os.CreateTemp("", "klar_run_*")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmpFile.Name())
	targetFile := tmpFile.Name()
	if err := self.Compile(unit, targetFile); err != nil {
		return nil, err
	}
	runCmd := exec.Command(targetFile)
	runCmd.Stdout = stdout
	runCmd.Stderr = stderr
	if err = runCmd.Run(); err != nil {
		return runCmd, err
	}
	return runCmd, nil
}

func (self *Compiler) Compile(unit CompilationUnit, targetFile string) error {
	tokens, err := token.Tokenize(unit.src, unit.file)
	if err != nil {
		return err
	}
	if self.OnTokenize != nil {
		(self.OnTokenize)(tokens)
	}
	fileParts := strings.Split(strings.Split(unit.file, ".")[0], "/")
	moduleName := fileParts[len(fileParts)-1]
	nodeCreator := ast.NewNodeCreator()
	module, err := ast.Parse(tokens, ast.Ident(moduleName), nodeCreator)
	if err != nil {
		return err
	}
	if self.OnParse != nil {
		if !(self.OnParse)(module) {
			return nil
		}
	}
	typeCreator := typed.NewTypeCreator()
	typeInfo, genericsResolver, err := typed.TypeCheck(module, typeCreator)
	if err != nil {
		return err
	}
	if self.OnTypeCheck != nil {
		if !(self.OnTypeCheck)(typeInfo) {
			return nil
		}
	}
	lowered := lower.Lower(module, typeInfo, genericsResolver, nodeCreator, typeCreator)
	if self.OnLowered != nil {
		if !(self.OnLowered)(lowered) {
			return nil
		}
	}
	irModule, err := ir.GenerateIR(lowered, codegen.DataLayout{})
	if err != nil {
		return err
	}
	if self.OnIR != nil {
		if !(self.OnIR)(irModule) {
			return nil
		}
	}
	asm, err := codegen.GenerateDarwinArm64ASM(irModule)
	if err != nil {
		return err
	}
	if self.OnASM != nil {
		if !(self.OnASM)(asm) {
			return nil
		}
	}
	buildCmd := exec.Command("clang", "-o", targetFile, "-x", "assembler", "-")
	buildCmd.Stdin = strings.NewReader(asm.String())
	buildCmd.Stdout = os.Stdout
	buildCmd.Stderr = os.Stderr
	if err = buildCmd.Run(); err != nil {
		return err
	}
	return nil
}
