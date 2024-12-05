package base

import (
	"fmt"
	"path/filepath"
	"runtime"
	"strings"
)

func Must(err error) {
	if err != nil {
		panic(fmt.Errorf("unexpected error: %w", err))
	}
}

func Debug(pairs ...interface{}) {
	pc, file, line, _ := runtime.Caller(1)
	fn := runtime.FuncForPC(pc).Name()
	if idx := strings.LastIndex(fn, "."); idx >= 0 {
		fn = fn[idx+1:]
	}
	prefix := fmt.Sprintf("[DEBUG] %s:%d %s()", filepath.Base(file), line, fn)
	needsNewlines := false
	lineLength := len(prefix)
	for i := 0; i < len(pairs); i += 2 {
		if i >= len(pairs)-1 {
			break
		}
		name := pairs[i].(string)
		val := pairs[i+1]
		valStr := fmt.Sprintf("%v", val)
		lineLength += len(name) + 2 + len(valStr) + 1

		if lineLength > 80 || strings.Contains(valStr, "\n") {
			needsNewlines = true
			break
		}
	}
	if !needsNewlines {
		fmt.Print(prefix)
		for i := 0; i < len(pairs); i += 2 {
			if i >= len(pairs)-1 {
				break
			}
			name := pairs[i].(string)
			val := pairs[i+1]
			if strings.HasSuffix(name, ":T") {
				fmt.Printf(" %s: %v", strings.Split(name, ":")[0], val)
				fmt.Printf(" (%T)", val)
			} else {
				fmt.Printf(" %s: %v", name, val)
			}
		}
		fmt.Println()
		return
	}
	fmt.Println(prefix)
	for i := 0; i < len(pairs); i += 2 {
		if i >= len(pairs)-1 {
			break
		}
		name := pairs[i].(string)
		print_type := strings.HasSuffix(name, ":T")
		if print_type {
			name = strings.Split(name, ":")[0]
		}
		val := pairs[i+1]
		fmt.Printf("    %s:\n", name)
		valStr := fmt.Sprintf("%v", val)
		if print_type {
			valStr = fmt.Sprintf("%v\n(%T)", val, val)
		}
		for _, line := range strings.Split(valStr, "\n") {
			fmt.Printf("        %s\n", line)
		}
	}
}
