package base

import "fmt"

func Must(err error) {
	if err != nil {
		panic(fmt.Errorf("unexpected error: %w", err))
	}
}
