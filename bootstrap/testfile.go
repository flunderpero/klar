package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/pkg/errors"
)

func runTestFile(file string, update bool) error {
	fmt.Printf("%s: ", file)
	src, err := os.ReadFile(file)
	if err != nil {
		fmt.Printf("FAIL\nError: %v\n", err)
		return err
	}
	expectStdout := ""
	expectExitCode := 0
	srcWithoutExpect := string(src)
	parts := strings.Split(string(src), "\n")
	if parts[0] == "-- expect.stdout {" {
		for i, part := range parts[1:] {
			if part == "-- } expect.stdout" {
				srcWithoutExpect = strings.Join(parts[i+4:], "\n")
				if strings.Index(parts[i+2], "-- expect.exit_code: ") == 0 {
					code, _ := strconv.ParseInt(parts[i+2][len("-- expect.exit_code: "):], 10, 32)
					expectExitCode = int(code)
				}
				break
			}
			if len(expectStdout) > 0 {
				expectStdout += "\n"
			}
			expectStdout += part[3:]
		}
	}
	compiler := Compiler{}
	stdout := strings.Builder{}
	stderr := strings.Builder{}
	runCmd, err := compiler.CompileAndRun(CompilationUnit{[]byte(srcWithoutExpect), file}, &stdout, &stderr)
	if err != nil && runCmd == nil {
		fmt.Printf("FAIL\nError: %v\n", err)
		return err
	}
	if update {
		src = []byte(fmt.Sprintf(
			"-- expect.stdout {\n-- %s\n-- } expect.stdout\n-- expect.exit_code: %d\n\n%s",
			strings.ReplaceAll(stdout.String(), "\n", "\n-- "),
			runCmd.ProcessState.ExitCode(),
			srcWithoutExpect))
		if err := os.WriteFile(file, src, 0644); err != nil {
			return errors.Wrapf(err, "Failed to update %q", file)
		}
		fmt.Println("OK (updated)")
	} else if runCmd.ProcessState.ExitCode() != expectExitCode {
		fmt.Printf("FAIL\nExpected exit code: %d\nGot: %d\n", expectExitCode, runCmd.ProcessState.ExitCode())
		return fmt.Errorf("Expectation failed")
	} else if stdout.String() != expectStdout {
		fmt.Printf("FAIL\nExpected:\n%s\nGot:\n%s\n", expectStdout, stdout.String())
		return fmt.Errorf("Expectation failed")
	} else {
		fmt.Println("OK")
	}
	return nil
}

func runTestFiles(dir string, update bool) error {
	files, err := os.ReadDir(dir)
	if err != nil {
		return err
	}
	overall := 0
	failed := 0
	for _, entry := range files {
		if entry.IsDir() {
			continue
		}
		if !strings.HasSuffix(entry.Name(), ".kl") {
			continue
		}
		overall += 1
		if err := runTestFile(dir+"/"+entry.Name(), update); err != nil {
			failed += 1
		}
	}
	if failed > 0 {
		fmt.Printf("Failed %d of %d tests in %q\n", failed, overall, dir)
	} else {
		fmt.Printf("All %d tests in %q passed\n", overall, dir)
	}
	return nil
}
