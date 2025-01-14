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

func BreakIfMultiline(s string, level int) string {
	if strings.Contains(s, "\n") {
		return "\n" + IndentString(s, level)
	}
	return s
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

func IndentCommaSlice[T fmt.Stringer](s []T, level int) string {
	var sb strings.Builder
	for i, f := range s {
		if i > 0 {
			sb.WriteString(",")
		}
		sb.WriteString("\n")
		sb.WriteString(Indent(f, level))
	}
	return sb.String()
}

func IndentStringSlice(s []string, level int) string {
	var sb strings.Builder
	for _, f := range s {
		sb.WriteString("\n")
		sb.WriteString(IndentString(f, level))
	}
	return sb.String()
}

func CommaSlice[T fmt.Stringer](s []T) string {
	var sb strings.Builder
	for i, f := range s {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(f.String())
	}
	return sb.String()
}

func PipeSlice[T fmt.Stringer](s []T) string {
	var sb strings.Builder
	for i, f := range s {
		if i > 0 {
			sb.WriteString(" | ")
		}
		sb.WriteString(f.String())
	}
	return sb.String()
}

func Map[F any, T any](values []F, f func(v F) T) []T {
	result := make([]T, len(values))
	for i, v := range values {
		result[i] = f(v)
	}
	return result

}
