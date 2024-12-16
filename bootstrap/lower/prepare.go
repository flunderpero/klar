package lower

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type funcInfo struct {
	funcDef  *ast.FunctionDefinition
	funcType *typed.FunctionType
}

type prepare struct {
	DefaultTransformer
	funcInfos                          map[typed.TypeId]*funcInfo
	funcInfo                           *funcInfo
	typeInfo                           *typed.TypeInfo
	genericsResolver                   *typed.GenericsResolver
	nodeCreator                        *ast.NodeCreator
	typeCreator                        *typed.TypeCreator
	tupleStructs                       map[string]*typed.StructType
	replaceTupleTypeWithStructTypeSeen map[typed.TypeId]typed.Type
	mergedReceiverGenericsFunctions    []*typed.FunctionType
}

func (self *prepare) convertToTypeIdIdentExpression(expr ast.Expression, ty typed.Type) *ast.IdentExpression {
	symbol := self.typeInfo.MustLookupSymbol(ty.Id())
	res := self.nodeCreator.NewIdentExpression(ast.Ident(symbol.FQN()), expr.Span())
	self.typeInfo.Set(res, ty)
	return res
}

func (self *prepare) mergeReceiverGenericsIntoFunction(funcType *typed.FunctionType, receiverType typed.GenericType) *typed.FunctionType {
	funcBase := funcType
	if base, ok := funcType.GenericBase(); ok {
		funcBase = base.(*typed.FunctionType)
	}
	typeArgs := receiverType.TypeArgs()
	for _, merged := range self.mergedReceiverGenericsFunctions {
		mergedBase := merged
		if base, ok := merged.GenericBase(); ok {
			mergedBase = base.(*typed.FunctionType)
		}
		if mergedBase.Id() != funcBase.Id() {
			continue
		}
		if len(merged.TypeArgs()) == len(typeArgs) && typed.MatchTypeArgs(merged, typeArgs) {
			return merged
		}
	}
	res := self.genericsResolver.CloneAndMergeReceiverGenerics(funcType, receiverType)
	self.mergedReceiverGenericsFunctions = append(self.mergedReceiverGenericsFunctions, res)
	return res
}

func (self *prepare) VisitMemberExpression(expr *ast.MemberExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkMemberExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.MemberExpression)
	if !ok {
		return newExpr, false
	}
	switch ty := self.typeInfo.MustLookup(expr).(type) {
	case *typed.FunctionType:
		if !ty.IsStaticMethod() {
			return expr, true
		}
		receiver := expr.Target
		receiverType := self.typeInfo.MustLookup(receiver).(typed.GenericType)
		if typed.HasTypeParams(receiverType) {
			ty = self.mergeReceiverGenericsIntoFunction(ty, receiverType)
		}
		res := self.convertToTypeIdIdentExpression(expr, ty)
		return res, true
	}
	return expr, true
}

func (self *prepare) VisitCallExpression(expr *ast.CallExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkCallExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.CallExpression)
	if !ok {
		return newExpr, true
	}
	calleeType := self.typeInfo.MustLookup(expr.Callee).(typed.CallableType)
	params := calleeType.CallParams()
	// Re-order the call arguments to be in the order of the function parameters.
	callArgs := make([]ast.CallArg, len(expr.Args))
	positionalArgIndex := 0
	for _, callArg := range expr.Args {
		if callArg.Name == "" {
			callArgs[positionalArgIndex] = callArg
			positionalArgIndex += 1
			continue
		}
		for i, param := range params {
			if param.Name == callArg.Name {
				callArgs[i] = callArg
				break
			}
		}
	}
	expr.Args = callArgs
	switch calleeType := calleeType.(type) {
	case *typed.FunctionType:
		if !calleeType.IsMethod() || calleeType.IsStaticMethod() {
			return expr, true
		}
		receiver := expr.Callee.(*ast.MemberExpression).Target
		receiverType := self.typeInfo.MustLookup(receiver).(typed.GenericType)
		if typed.HasTypeParams(receiverType) {
			// Propagate the type parameters and arguments from the receiver type to the callee.
			calleeType = self.mergeReceiverGenericsIntoFunction(calleeType, receiverType)
		}
		receiverCallArg := ast.CallArg{Name: "self", Value: receiver, Span: expr.Span()}
		expr.Args = append([]ast.CallArg{receiverCallArg}, expr.Args...)
		expr.Callee = self.convertToTypeIdIdentExpression(expr.Callee, calleeType)
		return expr, true
	case *typed.StructType:
		expr.Callee = self.convertToTypeIdIdentExpression(expr.Callee, calleeType)
		return expr, true
	default:
		panic(fmt.Sprintf("unexpected callable type: %T", calleeType))
	}
}

func (self *prepare) VisitFunctionDefinition(def *ast.FunctionDefinition, w TransformWalker) (*ast.FunctionDefinition, bool) {
	funcType := self.typeInfo.MustLookup(def.Decl).(*typed.DeclaredType).Type.(*typed.FunctionType)
	funcInfo := &funcInfo{funcDef: def, funcType: funcType}
	self.funcInfos[funcType.Id()] = funcInfo
	self.funcInfo = funcInfo
	def, ok := w.WalkFunctionDefinition(def)
	if !ok {
		return nil, false
	}
	self.funcInfo = nil
	return def, true
}

func (self *prepare) VisitTraitDeclaration(decl *ast.TraitDeclaration, w TransformWalker) (*ast.TraitDeclaration, bool) {
	w.WalkTraitDeclaration(decl)
	return nil, false
}

func (self *prepare) VisitImplDefinition(def *ast.ImplDefinition, w TransformWalker) (*ast.ImplDefinition, bool) {
	w.WalkImplDefinition(def)
	return nil, false
}

func (self *prepare) convertTupleToStructType(tupleType *typed.TupleType) *typed.StructType {
	key := ""
	for _, value := range tupleType.Values {
		key += value.Id().String() + ","
	}
	structType, ok := self.tupleStructs[key]
	if !ok {
		fields := make([]typed.TypeAndName[typed.Type], len(tupleType.Values))
		for i, value := range tupleType.Values {
			if tupleValue, ok := value.(*typed.TupleType); ok {
				value = self.convertTupleToStructType(tupleValue)
			}
			field := typed.TypeAndName[typed.Type]{Name: ast.Ident(fmt.Sprintf("%d", i)), Type: value}
			fields[i] = field
		}
		structType = self.typeCreator.NewStructType(nil, nil, nil, fields, nil, nil)
		if symbol, ok := self.typeInfo.LookupSymbol(tupleType.Id()); ok {
			self.typeInfo.DeclareSymbol(structType.Id(), symbol)
		}
		self.tupleStructs[key] = structType
	}
	return structType
}

func (self *prepare) VisitTupleLiteralExpression(expr *ast.TupleLiteralExpression, w TransformWalker) (ast.Expression, bool) {
	newExpr, ok := w.WalkTupleLiteralExpression(expr)
	if !ok {
		return nil, false
	}
	expr, ok = newExpr.(*ast.TupleLiteralExpression)
	if !ok {
		return newExpr, true
	}
	tupleType := self.typeInfo.MustLookup(expr).(*typed.TupleType)
	structType := self.convertTupleToStructType(tupleType)
	callArgs := make([]ast.CallArg, len(expr.Values))
	for i, value := range expr.Values {
		self.typeInfo.Set(value, structType.Fields[i].Type)
		callArgs[i] = ast.CallArg{Value: value, Span: value.Span()}
	}
	calleeExpr := self.nodeCreator.NewIdentExpression(ast.Ident("$Tuple"), expr.Span())
	self.typeInfo.Set(calleeExpr, structType)
	callExpr := self.nodeCreator.NewCallExpression(calleeExpr, callArgs, expr.Span())
	self.typeInfo.Set(callExpr, structType)
	return callExpr, true
}

func (self *prepare) VisitNode(node ast.Node, w TransformWalker) (ast.Node, bool) {
	node, ok := w.WalkNode(node)
	if !ok {
		return nil, false
	}
	ty := self.typeInfo.MustLookup(node)
	ty = self.replaceTupleTypeWithStructType(ty)
	self.typeInfo.Set(node, ty)
	return node, true
}

func (self *prepare) replaceTupleTypeWithStructType(ty typed.Type) typed.Type {
	// todo: `typed.DeclaredType` should not have the same `TypeId` as its enclosed type.
	if _, ok := ty.(*typed.DeclaredType); ok {
		return ty
	}
	if res, ok := self.replaceTupleTypeWithStructTypeSeen[ty.Id()]; ok {
		return res
	}
	switch tyKind := ty.(type) {
	case *typed.TupleType:
		ty = self.convertTupleToStructType(tyKind)
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
	case *typed.FunctionType:
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
		for i, param := range tyKind.Params {
			param.Type = self.replaceTupleTypeWithStructType(param.Type)
			tyKind.Params[i] = param
		}
		tyKind.Result = self.replaceTupleTypeWithStructType(tyKind.Result)
		if tyKind.Receiver != nil {
			tyKind.Receiver = self.replaceTupleTypeWithStructType(tyKind.Receiver)
		}
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
	case *typed.StructType:
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
		for i, field := range tyKind.Fields {
			field.Type = self.replaceTupleTypeWithStructType(field.Type)
			tyKind.Fields[i] = field
		}
		for i, method := range tyKind.Methods {
			method.Type = self.replaceTupleTypeWithStructType(method.Type).(*typed.FunctionType)
			tyKind.Methods[i] = method
		}
	case *typed.UnionType:
		self.replaceTupleTypeWithStructTypeSeen[ty.Id()] = ty
		for _, variant := range tyKind.Variants {
			switch variant.Kind {
			case typed.UnionVariantKindType:
				variant.Type = self.replaceTupleTypeWithStructType(variant.Type)
			case typed.UnionVariantKindNamed:
				variant.Named.Type = self.replaceTupleTypeWithStructType(variant.Named.Type)
			}
		}
	case *typed.BoolType,
		*typed.Int64Type,
		*typed.Int32Type,
		*typed.Int16Type,
		*typed.Int8Type,
		*typed.StrType,
		*typed.NoneType,
		*typed.NeverType,
		*typed.RawPtr,
		*typed.TypeParam:
	default:
		panic(fmt.Sprintf("unexpected type: %T", ty))
	}
	return ty
}

func Prepare(
	module *ast.Module,
	typeInfo *typed.TypeInfo,
	genericsResolver *typed.GenericsResolver,
	nodeCreator *ast.NodeCreator,
	typeCreator *typed.TypeCreator,
) (*ast.Module, map[typed.TypeId]*funcInfo) {
	prepare := &prepare{
		funcInfos:                          map[typed.TypeId]*funcInfo{},
		typeInfo:                           typeInfo,
		genericsResolver:                   genericsResolver,
		nodeCreator:                        nodeCreator,
		typeCreator:                        typeCreator,
		tupleStructs:                       map[string]*typed.StructType{},
		replaceTupleTypeWithStructTypeSeen: map[typed.TypeId]typed.Type{},
	}
	walker := DefaultTransformWalker{Transformer: prepare}
	module, ok := walker.Transformer.VisitModule(module, &walker)
	if !ok {
		panic("module has been deleted")
	}
	return module, prepare.funcInfos
}
