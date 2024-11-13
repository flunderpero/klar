package base

import (
	"fmt"
	"strings"
)

func IndentString(s string, level int) string {
	indent := strings.Repeat("    ", level)
	s = strings.ReplaceAll(s, "\n", "\n"+indent)
	return indent + s
}

func Indent(f fmt.Stringer, level int) string {
	return IndentString(f.String(), level)
}

func IndentSlice[T fmt.Stringer](s []T, level int) string {
	var sb strings.Builder
	for _, f := range s {
		sb.WriteString("\n")
		sb.WriteString(Indent(f, level))
	}
	return sb.String()
}
