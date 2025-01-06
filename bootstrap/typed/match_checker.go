/*
# Match Expression Usefulness And Exhaustiveness Check

This is a very naive approach to testing match expressions. It is only a stub for
future development.
*/
package typed

import (
	"fmt"
	"slices"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/pkg/errors"
)

type value interface{}

type intRange struct {
	from int64
	to   int64
}

type matchChecker struct {
	typeInfo         *TypeInfo
	matchedValueType Type
	patterns         []ast.MatchPattern
}

type wildcard struct{}

func CheckMatch(match *ast.MatchExpression, typeInfo *TypeInfo) error {
	patterns := []ast.MatchPattern{}
	for _, arm := range match.Arms {
		patterns = append(patterns, arm.Pattern)
	}
	checker := &matchChecker{
		typeInfo:         typeInfo,
		matchedValueType: typeInfo.MustLookup(match.Expression),
		patterns:         patterns,
	}
	return checker.specialize(checker.matchedValueType)
}

func (self *matchChecker) specialize(ty Type) error {
	values := []value{}
	for _, pattern := range self.patterns {
		v := self.patternValue(pattern)
		if _, ok := v.(*wildcard); ok {
			return nil
		}
		values = append(values, v)
	}
	return self.isExhaustive(ty, values)
}

func (self *matchChecker) isExhaustive(ty Type, values []value) error {
	switch ty.(type) {
	case *UInt8Type:
		return self.isExhaustiveInt(values, 0, 255)
	case *UnionType:
		return self.isExhaustiveUnion(ty, values)
	case *CharType:
		return self.isExhaustiveInt(values, 0, 0x10FFFF)
	}
	switch ty.Id() {
	case self.typeInfo.BuiltIns.Str.Id():
		return errors.Errorf("string match is only exhaustive if it contains the wildcard pattern")
	}
	panic(fmt.Sprintf("unhandled type %T", ty))
}

func (self *matchChecker) isExhaustiveInt(values []value, min int64, max int64) error {
	slices.SortFunc(values, func(a value, b value) int {
		av := a.(intRange)
		bv := b.(intRange)
		if av.from < bv.from {
			return -1
		}
		if av.from > bv.from {
			return 1
		}
		return 0
	})
	var minVal int64 = min
	for _, v_ := range values {
		v := v_.(intRange)
		if v.from > minVal {
			return fmt.Errorf("missing pattern for %d..%d", minVal, v.from-1)
		}
		minVal = v.to + 1
	}
	if minVal <= max {
		return fmt.Errorf("missing pattern for %d..%d", minVal, max)
	}
	return nil
}

func (self *matchChecker) isExhaustiveUnion(ty Type, values []value) error {
	unionType := ty.(*UnionType)
	missingVariants := map[TypeId]Type{}
	for _, variant := range unionType.Variants {
		missingVariants[variant.AsType().Id()] = variant.AsType()
	}
	for _, v_ := range values {
		v := v_.(Type)
		delete(missingVariants, v.Id())
	}
	if len(missingVariants) > 0 {
		missing := ""
		for _, variant := range missingVariants {
			missing += variant.String() + " "
		}
		return fmt.Errorf("missing patterns for %s", missing)
	}
	return nil
}

func (self *matchChecker) patternValue(pattern ast.MatchPattern) value {
	switch pattern := pattern.(type) {
	case *ast.IntPattern:
		return intRange{pattern.Value.Int64, pattern.Value.Int64} // fixme uint64
	case *ast.IntRangePattern:
		from := pattern.From.Int64 // fixme uint64
		to := pattern.To.Int64     // fixme uint64
		if !pattern.InclusiveTo {
			to -= 1
		}
		return intRange{from, to}
	case *ast.CharPattern:
		return intRange{int64(pattern.Value.Value), int64(pattern.Value.Value)}
	case *ast.CharRangePattern:
		from := int64(pattern.From.Value)
		to := int64(pattern.To.Value)
		if !pattern.InclusiveTo {
			to -= 1
		}
		return intRange{from, to}
	case *ast.UnionTypePattern:
		return self.typeInfo.MustLookup(pattern)
	case *ast.StrPattern:
		// We don't need to do anything, because a string match is only exhaustive if it
		// contains the wildcard pattern.
		return nil
	case *ast.WildcardPattern:
		return &wildcard{}
	}
	panic(fmt.Sprintf("unhandled pattern %T", pattern))
}
