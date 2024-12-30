package typed

import (
	"fmt"
	"testing"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/token"
)

func TestSimpleCase(t *testing.T) {
	t.Run("Int ranges", func(t *testing.T) {
		if err := typeCheck(`
            let x U8 = 100
            match x {
                case 201..255 {}
                case 0..200 {}
            }`); err != nil {
			t.Error(err)
		}
		if err := typeCheck(`
            let x U8 = 100
            match x {
                case 0..200 {}
                case 201 {}
                case 220..255 {}
            }`); err == nil {
			t.Error("exhaustiveness check should have failed")
		}
	})
	t.Run("Union", func(t *testing.T) {
		if err := typeCheck(`
            union StrBoolColor = Str | Bool | .Green
            let x StrBoolColor = "test"
            match x {
                case Str {}
                case Bool {}
                case .Green {}
            }`); err != nil {
			t.Error(err)
		}
		if err := typeCheck(`
            union StrBoolColor = Str | Bool | .Green
            let x StrBoolColor = "test"
            match x {
                case Str {}
                case .Green {}
            }`); err == nil {
			t.Error("exhaustiveness check should have failed")
		}
	})
	t.Run("Wildcard", func(t *testing.T) {
		if err := typeCheck(`
            let x U8 = 100
            match x {
                case 1 {}
                case _ {}
            }`); err != nil {
			t.Error(err)
		}
	})
}

func typeCheck(src string) error {
	src = fmt.Sprintf("fn main() {\n%s}", src)
	tokens, err := token.Tokenize([]byte(src), "test.kl")
	if err != nil {
		return err
	}
	nodeCreator := ast.NewNodeCreator()
	module, err := ast.Parse(tokens, ast.Ident("test"), nodeCreator)
	if err != nil {
		return err
	}
	typeCreator := NewTypeCreator()
	_, _, err = TypeCheck(module, typeCreator)
	return err
}
