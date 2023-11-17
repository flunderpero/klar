import {HasKindAndSpan, quote, Span} from "./common"
import {Identifier, LexicalToken, NumberToken, Token} from "./lexer"

type Mark = number

export class TokenStream {
    index = 0

    constructor(public tokens: Token[]) {}

    at_end() {
        return this.index >= this.tokens.length
    }

    peek(): Token | undefined {
        return this.tokens[this.index]
    }

    consume() {
        if (this.at_end()) {
            throw new ParseError("Unexpected EOF", this.tokens[this.index - 1]?.span)
        }
        return this.tokens[this.index++]
    }

    expect_identifier() {
        const token = this.peek()
        if (!token) {
            throw new ParseError(
                "Expected identifier but got EOF",
                this.tokens[this.index - 1]?.span,
            )
        }
        if (!(token instanceof Identifier)) {
            throw new ParseError(
                `Expected identifier but got ${quote(token.toString())}`,
                token.span,
            )
        }
        return this.consume() as Identifier
    }

    expect(token_kind: typeof LexicalToken.prototype.value) {
        let actual = this.peek()
        if (!actual) {
            throw new ParseError(
                `Expected token ${quote(token_kind)} but got EOF`,
                this.tokens[this.index - 1]?.span,
            )
        }
        if (!(actual instanceof LexicalToken) || actual.value !== token_kind) {
            throw new ParseError(
                `Expected token ${quote(token_kind)} but got ${quote(actual.toString())}`,
                actual.span,
            )
        }
        return this.consume()
    }

    simple_peek() {
        const token = this.peek()
        return token instanceof LexicalToken ? token.value : undefined
    }

    mark(): Mark {
        return this.index
    }

    reset(mark: Mark) {
        this.index = mark
    }
}

export class ParseError extends Error {
    constructor(
        public error: string,
        public span: Span | undefined,
    ) {
        super(`${error} at ${span ?? "EOF"}`)
    }
}

export abstract class ASTNode extends HasKindAndSpan {
    kind = "ast node"

    constructor(span: Span) {
        super(span)
    }

    contained_types(): Type[] {
        return []
    }

    contained_nodes(): ASTNode[] {
        return []
    }
}

export abstract class Expression extends ASTNode {
    kind = "expression"
    constructor(span: Span) {
        super(span)
    }
}

export class FunctionDefinition extends Expression {
    kind = "function definition"
    declaration: FunctionDeclaration
    block: Block
    extern?: boolean

    constructor(
        data: {
            declaration: FunctionDeclaration
            block: Block
            extern?: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionDefinition.prototype)
    }

    get name() {
        return this.declaration.name
    }

    to_signature_string() {
        return this.declaration.to_signature_string()
    }

    contained_nodes() {
        return [this.block]
    }
}

abstract class DeclarationOrDefinition extends ASTNode {
    kind = "declaration block"
    contained_declarations(): DeclarationOrDefinition[] {
        return []
    }
}

export class StructDeclaration extends DeclarationOrDefinition {
    kind = "type declaration"
    name: string
    members: Record<string, Type>
    type_parameters: Type[]
    impls: ImplDefinition[] = []
    extern?: boolean

    constructor(
        data: {
            name: string
            members: Record<string, Type>
            type_parameters: Type[]
            extern?: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof StructDeclaration.prototype)
    }

    to_signature_string() {
        if (this.type_parameters.length === 0) {
            return this.name
        }
        return `${this.name}<${this.type_parameters.map((t) => t.name).join(", ")}>`
    }

    contained_types() {
        return Object.values(this.members).concat(this.type_parameters)
    }
}

export class TupleDeclaration extends DeclarationOrDefinition {
    kind = "tuple declaration"
    members: Type[]

    constructor(data: {members: Type[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof TupleDeclaration.prototype)
    }

    get name() {
        return ""
    }

    contained_types() {
        return this.members
    }

    to_signature_string() {
        return `(${this.members.map((t) => t.to_signature_string()).join(", ")})`
    }
}

export class Tuple extends Expression {
    kind = "tuple"
    values: Expression[]

    constructor(data: {values: Expression[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Tuple.prototype)
    }

    contained_nodes() {
        return this.values
    }
}

export class ParenthesizedExpression extends Expression {
    kind = "parenthesized expression"
    expression: Expression

    constructor(data: {expression: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof ParenthesizedExpression.prototype)
    }

    contained_nodes() {
        return [this.expression]
    }
}

export class EnumDeclaration extends DeclarationOrDefinition {
    kind = "enum declaration"
    name: string
    variants: Record<string, EnumVariant>
    type_parameters: Type[]
    impls: ImplDefinition[] = []
    extern?: boolean

    constructor(
        data: {
            name: string
            variants: Record<string, EnumVariant>
            type_parameters: Type[]
            extern?: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof EnumDeclaration.prototype)
    }

    to_signature_string() {
        if (this.type_parameters.length === 0) {
            return this.name
        }
        return `${this.name}<${this.type_parameters.map((t) => t.name).join(", ")}>`
    }

    contained_types() {
        return this.type_parameters
    }
}

class EnumVariant extends HasKindAndSpan {
    kind = "enum variant"
    name: string
    values: TupleDeclaration

    constructor(data: {name: string; values: TupleDeclaration}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof EnumVariant.prototype)
    }
}

export class BuiltInTypeDefinition extends HasKindAndSpan {
    kind = "builtin type definition"
    name: string

    constructor(name: string) {
        super(new Span(0, 0, "<builtin>", ""))
        this.name = name
    }

    to_signature_string() {
        return this.name
    }
}

type ResolvedType =
    | StructDeclaration
    | TraitDeclaration
    | FunctionDeclaration
    | BuiltInTypeDefinition
    | EnumDeclaration
    | TupleDeclaration

export class Type extends HasKindAndSpan {
    kind = "type"
    name: string
    type_parameters: Type[]
    resolved: ResolvedType
    is_type_parameter?: boolean

    constructor(
        data: {
            name: string
            type_parameters: Type[]
            resolved?: ResolvedType
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof Type.prototype)
    }

    to_signature_string(): string {
        if (this.type_parameters.length === 0) {
            return this.name
        } else {
            return `${this.name}<${this.type_parameters
                .map((t) => t.to_signature_string())
                .join(", ")}>`
        }
    }
}

export const type_needs_to_be_inferred = new Type(
    {name: "<needs-to-be-inferred>", type_parameters: []},
    new Span(0, 0, "<unknown>", ""),
)

export class Return extends Expression {
    kind = "return"
    value: Expression

    constructor(data: {value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Return.prototype)
    }

    contained_nodes(): ASTNode[] {
        return [this.value]
    }
}

export class Number_ extends Expression {
    kind = "number"
    value: string

    constructor(data: {value: string}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Number_.prototype)
    }
}

export class Bool extends Expression {
    kind = "bool"
    value: boolean

    constructor(data: {value: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Bool.prototype)
    }
}

export class Not extends Expression {
    kind = "not"
    value: Expression

    constructor(data: {value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Not.prototype)
    }

    contained_nodes(): ASTNode[] {
        return [this.value]
    }
}

export class FunctionCall extends Expression {
    kind = "function call"
    target: Expression
    arguments: Expression[]

    constructor(data: {target: Expression; arguments: Expression[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionCall.prototype)
    }

    contained_nodes() {
        return [this.target, ...this.arguments]
    }
}

export class StructInstantiation extends Expression {
    kind = "struct instantiation"
    type_arguments: Type[]
    target_struct_name: string
    values: Record<string, Expression>

    constructor(
        data: {
            type_arguments: Type[]
            target_struct_name: string
            values: Record<string, Expression>
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof StructInstantiation.prototype)
    }

    contained_types() {
        return this.type_arguments
    }

    contained_nodes() {
        return Object.values(this.values)
    }
}

export class VariableDeclaration extends Expression {
    kind = "variable declaration"
    name: string
    value?: Expression
    type: Type
    mutable: boolean

    constructor(
        data: {
            name: string
            value?: Expression
            type: Type
            mutable: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof VariableDeclaration.prototype)
    }

    contained_types() {
        return [this.type]
    }

    contained_nodes() {
        return this.value ? [this.value] : []
    }
}

export class IdentifierReference extends Expression {
    kind = "identifier reference"
    name: string
    type_parameters: Type[]
    resolved: VariableDeclaration | StructDeclaration | FunctionDeclaration | EnumDeclaration

    constructor(data: {name: string; type_parameters: Type[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof IdentifierReference.prototype)
    }

    contained_types() {
        return this.type_parameters
    }
}

export class Assignment extends Expression {
    kind = "assignment"
    target: Expression
    value: Expression

    constructor(data: {target: Expression; value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Assignment.prototype)
    }

    contained_nodes() {
        return [this.target, this.value]
    }
}

export class FieldAccess extends Expression {
    kind = "field access"
    target: Expression
    field: string | number

    constructor(data: {target: Expression; field: string | number}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FieldAccess.prototype)
    }

    contained_nodes() {
        return [this.target]
    }
}

type BinaryOperator = "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "and" | "or"

export class BinaryExpression extends Expression {
    static precedence: Record<BinaryOperator, number> = {
        or: 1,
        and: 2,
        "==": 3,
        "!=": 3,
        "<": 4,
        "<=": 4,
        ">": 4,
        ">=": 4,
        "+": 5,
        "-": 5,
        "*": 6,
        "/": 6,
    }
    kind = "binary"
    operator: BinaryOperator
    lhs: Expression
    rhs: Expression

    constructor(data: {operator: BinaryOperator; lhs: Expression; rhs: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof BinaryExpression.prototype)
    }

    contained_nodes() {
        return [this.lhs, this.rhs]
    }
}

export class If extends Expression {
    kind = "if"
    condition: Expression
    then_block: Block
    else_block?: Block

    constructor(data: {condition: Expression; then_block: Block; else_block?: Block}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof If.prototype)
    }

    contained_nodes() {
        return [this.condition, this.then_block, ...(this.else_block ? [this.else_block] : [])]
    }
}

export class Match extends Expression {
    kind = "match"
    value: Expression
    arms: MatchArm[]
    wildcard_block?: Block

    constructor(data: {value: Expression; arms: MatchArm[]; wildcard_block?: Block}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Match.prototype)
    }

    contained_nodes() {
        return [this.value, ...this.arms, ...(this.wildcard_block ? [this.wildcard_block] : [])]
    }
}

export class MatchArm extends ASTNode {
    kind = "match arm"
    pattern: MatchPattern
    block: Block

    constructor(data: {pattern: MatchPattern; block: Block}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof MatchArm.prototype)
    }

    contained_nodes() {
        return [this.pattern, this.block]
    }
}

export abstract class MatchPattern extends ASTNode {
    kind = "match pattern"

    constructor(span: Span) {
        super(span)
    }
}

export class LiteralMatchPattern extends MatchPattern {
    kind = "literal number match pattern"
    value: number | boolean

    constructor(data: {value: number | boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof LiteralMatchPattern.prototype)
    }
}

export class WildcardMatchPattern extends MatchPattern {
    kind = "wildcard match pattern"

    constructor(span: Span) {
        super(span)
    }
}

export class CaptureMatchPattern extends MatchPattern {
    kind = "capture match pattern"
    name: string

    constructor(data: {name: string}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof CaptureMatchPattern.prototype)
    }
}

export class StructuredMatchPattern extends MatchPattern {
    kind = "structured match pattern"
    type_expression: IdentifierReference | FieldAccess
    fields: Record<string, MatchPattern>

    constructor(
        data: {type_expression: Expression; fields: Record<string, MatchPattern>},
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof StructuredMatchPattern.prototype)
    }

    contained_nodes() {
        return [this.type_expression, ...Object.values(this.fields)]
    }
}

export class TupleMatchPattern extends MatchPattern {
    kind = "tuple match pattern"
    type_expression: IdentifierReference | FieldAccess
    values: MatchPattern[]

    constructor(data: {type_expression: Expression; values: MatchPattern[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof TupleMatchPattern.prototype)
    }

    contained_nodes() {
        return [this.type_expression, ...this.values]
    }
}

export class Loop extends Expression {
    kind = "loop"
    block: Block

    constructor(data: {block: Block}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Loop.prototype)
    }

    contained_nodes() {
        return [this.block]
    }
}

export class Break extends Expression {
    kind = "break"

    constructor(span: Span) {
        super(span)
    }
}

export class Continue extends Expression {
    kind = "continue"

    constructor(span: Span) {
        super(span)
    }
}

export class Parameter extends HasKindAndSpan {
    kind = "parameter"
    name: string
    type: Type
    mutable: boolean

    constructor(data: {name: string; type: Type; mutable: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Parameter.prototype)
    }

    to_signature_string() {
        return `${this.name} ${this.type.to_signature_string()}`
    }

    contained_types() {
        return [this.type]
    }
}

export class Block extends DeclarationOrDefinition {
    kind = "block"
    body: ASTNode[]

    constructor(data: {body: Expression[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Block.prototype)
    }

    contained_nodes() {
        return this.body
    }
}

export class FunctionDeclaration extends DeclarationOrDefinition {
    kind = "function declaration"
    name: string
    parameters: Parameter[]
    return_type: Type

    constructor(data: {name: string; parameters: Parameter[]; return_type: Type}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionDeclaration.prototype)
    }

    to_signature_string() {
        return `${this.name}(${this.parameters
            .map((p) => `${p.name}: ${p.type.to_signature_string()}`)
            .join(", ")}): ${this.return_type.to_signature_string()}`
    }

    contained_types() {
        return [this.return_type, ...this.parameters.map((p) => p.type)]
    }
}

export class ExternBlock extends DeclarationOrDefinition {
    kind = "extern block"
    functions: FunctionDeclaration[] = []
    structs: StructDeclaration[] = []
    impls: ImplDefinition[] = []

    constructor(span: Span) {
        super(span)
    }

    contained_declarations() {
        return [...this.functions, ...this.structs, ...this.impls] as DeclarationOrDefinition[]
    }
}

export class ImplDefinition extends DeclarationOrDefinition {
    kind = "impl_definition"
    trait_name?: string
    type_parameters: Type[]
    resolved_trait?: TraitDeclaration
    target_name: string
    resolved_target: StructDeclaration | EnumDeclaration
    member_functions: FunctionDefinition[]
    static_functions: FunctionDefinition[]

    constructor(
        data: {
            trait_name?: string
            type_parameters: Type[]
            target_name: string
            member_functions: FunctionDefinition[]
            static_functions: FunctionDefinition[]
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof ImplDefinition.prototype)
    }

    contained_nodes(): ASTNode[] {
        return [...this.member_functions, ...this.static_functions]
    }

    contained_types(): Type[] {
        return this.type_parameters
    }
}

export class TraitDeclaration extends DeclarationOrDefinition {
    kind = "trait declaration"
    name: string
    member_function_declarations: FunctionDeclaration[]
    static_function_declarations: FunctionDeclaration[]
    member_function_default_impls: Record<string, FunctionDefinition>
    static_function_default_impls: Record<string, FunctionDefinition>
    type_parameters: Type[]

    constructor(
        data: {
            name: string
            member_function_declarations: FunctionDeclaration[]
            static_function_declarations: FunctionDeclaration[]
            member_function_default_impls: Record<string, FunctionDefinition>
            static_function_default_impls: Record<string, FunctionDefinition>
            type_parameters: Type[]
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof TraitDeclaration.prototype)
    }

    to_signature_string() {
        if (this.type_parameters.length === 0) {
            return this.name
        }
        return `${this.name}<${this.type_parameters
            .map((t) => t.to_signature_string())
            .join(", ")}>`
    }

    contained_nodes(): ASTNode[] {
        return [
            ...this.member_function_declarations,
            ...this.static_function_declarations,
            ...Object.values(this.member_function_default_impls),
            ...Object.values(this.static_function_default_impls),
        ]
    }
}

const self_type = new Type({name: "Self", type_parameters: []}, new Span(0, 0, "<builtin>", ""))

// Environment

export class Environment {
    outer?: Environment
    self_type: Type | undefined
    resolved_types = new Map<string, ResolvedType>()
    type_parameters = new Map<string, Type>()
    variables = new Map<string, VariableDeclaration>()
    on_scope_enter?: (env: Environment) => void

    constructor(outer?: Environment, on_scope_enter?: (env: Environment) => void) {
        this.outer = outer
        this.on_scope_enter = on_scope_enter || outer?.on_scope_enter
        if (outer) {
            this.on_scope_enter?.(this)
        }
    }

    add_type_parameter(type_parameter: Type) {
        this.type_parameters.set(type_parameter.name, type_parameter)
    }

    find_resolved_type(name: string): ResolvedType | undefined {
        return this.resolved_types.get(name) ?? this.outer?.find_resolved_type(name)
    }

    find_type_parameter(name: string): Type | undefined {
        return this.type_parameters.get(name) ?? this.outer?.find_type_parameter(name)
    }

    add_resolved(name: string, resolved: ResolvedType) {
        const existing = this.resolved_types.get(name)
        if (existing) {
            throw new ParseError(
                `${quote(name)} is already defined here: ${quote(
                    existing.to_signature_string(),
                )})} at ${existing.span}`,
                resolved.span,
            )
        }
        this.resolved_types.set(name, resolved)
    }

    resolve_type(type: Type): Type {
        if (type.resolved || type == type_needs_to_be_inferred) {
            return type
        }
        if (this.find_type_parameter(type.name)) {
            type.is_type_parameter = true
            return type
        }
        const resolved = this.find_resolved_type(type.name)
        if (!resolved) {
            throw new ParseError(`Type ${quote(type.name)} not found`, type.span)
        }
        type.resolved = resolved
        return type
    }

    find_variable(name: string): VariableDeclaration | undefined {
        return this.variables.get(name) ?? this.outer?.find_variable(name)
    }

    add_variable(name: string, variable: VariableDeclaration) {
        this.assert_name_is_not_already_defined(name, variable.span)
        this.variables.set(name, variable)
    }

    private assert_name_is_not_already_defined(name: string, span: Span) {
        const existing = this.find_variable(name) || this.find_resolved_type(name)
        if (existing) {
            const name =
                existing instanceof VariableDeclaration
                    ? existing.name
                    : existing.to_signature_string()
            throw new ParseError(
                `${quote(name)} is already defined here: ${quote(name)} at ${existing.span}`,
                span,
            )
        }
    }
}

export type AST = Block

/**
 * Parse tokens into an AST.
 */
export function parse(tokens: TokenStream, env: Environment): AST {
    const ast = parse_ignoring_types(tokens)
    return resolve_types_and_identifier_references(ast, env)
}

/**
 * Modify the AST to resolve all types.
 *
 * FEAT: All types explicitly specified in the source code are resolved.
 */
function resolve_types_and_identifier_references(block: Block, env: Environment) {
    forward_parse_declarations(block.body)
    for (const e of block.body) {
        resolve_for_node(e)
    }
    return block

    function resolve_for_node(e: ASTNode | DeclarationOrDefinition) {
        if (e instanceof Block) {
            resolve_types_and_identifier_references(e, new Environment(env))
        } else if (e instanceof FunctionDefinition) {
            resolve_for_node(e.declaration)
            const fn_env = new Environment(env)
            for (const p of e.declaration.parameters) {
                if (p.name === "self") {
                    if (!env.self_type) {
                        throw new ParseError(`${quote("self")} is not defined`, p.span)
                    }
                    if (
                        p.type.resolved &&
                        p.type.resolved.name !== self_type.name &&
                        p.type.resolved !== env.self_type
                    ) {
                        throw new ParseError(
                            `${quote("self")} has type ${quote(
                                p.type.to_signature_string(),
                            )} but expected ${quote(env.self_type.to_signature_string())}`,
                            p.span,
                        )
                    }
                    p.type = env.self_type
                }
                fn_env.add_variable(
                    p.name,
                    new VariableDeclaration(
                        {name: p.name, type: p.type, mutable: p.mutable},
                        p.span,
                    ),
                )
            }
            resolve_types_and_identifier_references(e.block, fn_env)
        } else if (e instanceof FunctionDeclaration) {
            env.resolve_type(e.return_type)
            for (const p of e.parameters) {
                if (p.name === "self" && p.type == type_needs_to_be_inferred) {
                    p.type = self_type
                } else {
                    env.resolve_type(p.type)
                }
            }
        } else if (e instanceof VariableDeclaration) {
            resolve_for_contained_types_and_nodes(e)
            env.add_variable(e.name, e)
        } else if (e instanceof ImplDefinition) {
            const prev_self_type = env.self_type
            env.self_type = env.resolve_type(
                new Type({name: e.target_name, type_parameters: []}, e.span),
            )
            resolve_for_contained_types_and_nodes(e)
            env.self_type = prev_self_type
        } else if (e instanceof IdentifierReference) {
            const resolved = env.find_variable(e.name) ?? env.find_resolved_type(e.name)
            if (!resolved) {
                throw new ParseError(`Identifier ${quote(e.name)} not found`, e.span)
            }
            if (
                resolved instanceof VariableDeclaration ||
                resolved instanceof StructDeclaration ||
                resolved instanceof EnumDeclaration ||
                resolved instanceof FunctionDeclaration
            ) {
                e.resolved = resolved
            } else {
                throw new ParseError(`Identifier ${quote(e.name)} is not a known type`, e.span)
            }
        } else if (e instanceof MatchArm) {
            const match_arm_env = new Environment(env)
            function resolve_captures_and_add_to_env(pattern: MatchPattern) {
                if (pattern instanceof StructuredMatchPattern) {
                    if (
                        pattern.type_expression instanceof IdentifierReference &&
                        !env.find_resolved_type(pattern.type_expression.name)
                    ) {
                        // This should be a capture.
                        pattern = new CaptureMatchPattern(
                            {name: pattern.type_expression.name},
                            pattern.span,
                        )
                    } else {
                        for (const name of Object.keys(pattern.fields)) {
                            pattern.fields[name] = resolve_captures_and_add_to_env(
                                pattern.fields[name],
                            )
                        }
                    }
                } else if (pattern instanceof TupleMatchPattern) {
                    for (let i = 0; i < pattern.values.length; i++) {
                        let value = pattern.values[i]
                        if (
                            value instanceof IdentifierReference &&
                            !env.find_resolved_type(value.name)
                        ) {
                            // This should be a capture.
                            pattern.values[i] = new CaptureMatchPattern(
                                {name: value.name},
                                value.span,
                            )
                        }
                        pattern.values[i] = resolve_captures_and_add_to_env(pattern.values[i])
                    }
                }
                if (pattern instanceof CaptureMatchPattern) {
                    match_arm_env.add_variable(
                        pattern.name,
                        new VariableDeclaration(
                            {name: pattern.name, type: type_needs_to_be_inferred, mutable: false},
                            pattern.span,
                        ),
                    )
                }
                return pattern
            }
            e.pattern = resolve_captures_and_add_to_env(e.pattern)
            resolve_for_node(e.pattern)
            resolve_types_and_identifier_references(e.block, match_arm_env)
        } else {
            resolve_for_contained_types_and_nodes(e)
        }
    }

    function resolve_for_contained_types_and_nodes(e: ASTNode) {
        for (const x of e.contained_types()) {
            env.resolve_type(x)
        }
        for (const x of e.contained_nodes()) {
            resolve_for_node(x)
        }
    }

    function forward_parse_declarations(expressions: Expression[]) {
        env = new Environment(env)
        const impls: ImplDefinition[] = []
        for (const e of expressions) {
            if (e instanceof FunctionDeclaration) {
                env.add_resolved(e.name, e)
            } else if (e instanceof FunctionDefinition) {
                env.add_resolved(e.declaration.name, e.declaration)
            } else if (
                e instanceof StructDeclaration ||
                e instanceof TraitDeclaration ||
                e instanceof EnumDeclaration
            ) {
                env.add_resolved(e.name, e)
                for (const x of e.type_parameters) {
                    env.add_type_parameter(x)
                }
            } else if (e instanceof ImplDefinition) {
                impls.push(e)
            } else if (e instanceof ExternBlock) {
                for (const x of e.functions) {
                    env.add_resolved(x.name, x)
                }
                for (const x of e.structs) {
                    env.add_resolved(x.name, x)
                }
                for (const x of e.impls) {
                    impls.push(x)
                }
            }
        }
        // FEAT: This allows `impl` blocks to be defined before the structs, enums and traits.
        for (const impl of impls) {
            const target = env.find_resolved_type(impl.target_name)
            if (!target) {
                throw new ParseError(
                    `Struct or enum ${quote(impl.target_name)} not found`,
                    impl.span,
                )
            }
            if (!(target instanceof StructDeclaration || target instanceof EnumDeclaration)) {
                throw new ParseError(
                    `${quote(impl.target_name)} is not a struct or an enum`,
                    impl.span,
                )
            }
            for (const x of impl.type_parameters) {
                env.add_type_parameter(x)
            }
            impl.resolved_target = target
            if (impl.trait_name) {
                const trait = env.find_resolved_type(impl.trait_name)
                if (!trait) {
                    throw new ParseError(`Trait ${quote(impl.trait_name)} not found`, impl.span)
                }
                if (!(trait instanceof TraitDeclaration)) {
                    throw new ParseError(`${quote(impl.trait_name)} is not a trait`, impl.span)
                }
                impl.resolved_trait = trait
            }
            target.impls.push(impl)
        }
    }
}

/**
 * Parse tokens into an AST.
 *
 * FEAT: The syntax is correct.
 */
function parse_ignoring_types(tokens: TokenStream): AST {
    if (tokens.at_end()) {
        return new Block({body: []}, new Span(0, 0, "<unknown>", ""))
    }
    const ast = new Block(
        {body: []},
        Span.combine(tokens.tokens[0].span, tokens.tokens.at(-1)!.span),
    )
    while (!tokens.at_end()) {
        ast.body.push(parse_expression())
    }
    return ast
    /**
     * Parse an expression and respect binary operator precedence.
     */
    function parse_expression(): Expression {
        function parse_binary_expression(min_precedence: number) {
            let lhs = parse_primary()
            while (true) {
                const op = tokens.simple_peek()
                const op_precedence = BinaryExpression.precedence[op as BinaryOperator]
                if (!op_precedence) {
                    if (op === "=") {
                        return parse_assignment(lhs)
                    }
                    return lhs
                }
                if (op_precedence >= min_precedence) {
                    tokens.consume()
                    // Parse the right-hand side of the expression
                    // with a higher precedence than this one.
                    // This way, we can parse expressions like
                    // `1 + 2 * 3` as `1 + (2 * 3)` instead of
                    // `(1 + 2) * 3`.
                    let rhs = parse_binary_expression(op_precedence + 1)
                    lhs = new BinaryExpression(
                        {operator: op as BinaryOperator, lhs, rhs},
                        Span.combine(lhs, rhs),
                    )
                } else {
                    return lhs
                }
            }
        }
        return parse_binary_expression(1)
    }

    function parse_primary(): Expression {
        const token = tokens.peek()
        if (!token) {
            throw new ParseError(`Unexpected EOF`, tokens.tokens[tokens.index - 1]?.span)
        }
        let expression: Expression | undefined
        if (token instanceof LexicalToken) {
            const simple_token = token.value
            if (simple_token === "fn") {
                expression = parse_function_definition()
            } else if (simple_token === "let" || simple_token === "mut") {
                expression = parse_variable_declaration()
            } else if (simple_token === "true" || simple_token === "false") {
                const span = tokens.consume().span
                expression = new Bool({value: simple_token === "true"}, span)
            } else if (simple_token === "not") {
                const span = tokens.consume().span
                const value = parse_expression()
                expression = new Not({value}, Span.combine(span, value))
            } else if (simple_token === "if") {
                expression = parse_if()
            } else if (simple_token === "loop") {
                expression = parse_loop()
            } else if (simple_token === "(") {
                expression = parse_parenthesized_expression_or_tuple()
            } else if (simple_token === "break") {
                const span = tokens.consume().span
                expression = new Break(span)
            } else if (simple_token === "continue") {
                const span = tokens.consume().span
                expression = new Continue(span)
            } else if (simple_token === "extern") {
                expression = parse_extern_block()
            } else if (simple_token === "struct") {
                expression = parse_struct_definition()
            } else if (simple_token === "enum") {
                expression = parse_enum_declaration()
            } else if (simple_token === "trait") {
                expression = parse_trait_definition()
            } else if (simple_token === "impl") {
                expression = parse_impl("all")
            } else if (simple_token === "return") {
                expression = parse_return_expression()
            } else if (simple_token === "match") {
                expression = parse_match_expression()
            }
        } else if (token instanceof NumberToken) {
            tokens.consume()
            expression = new Number_({value: token.value}, token.span)
        } else if (token instanceof Identifier) {
            const token = tokens.expect_identifier()
            expression =
                try_parse_function_call(token) ||
                try_parse_struct_initialization(token) ||
                parse_identifier_reference(token)
        }
        if (!expression) {
            throw new ParseError(`Unexpected token ${quote(token)}`, token.span)
        }
        while (true) {
            if (tokens.simple_peek() === "(") {
                expression = try_parse_function_call(expression) || expression
            } else if (tokens.simple_peek() === ".") {
                expression = parse_field_access(expression)
            } else {
                break
            }
        }
        return expression
    }

    function parse_assignment(target: Expression): Assignment {
        const span = tokens.consume().span
        const value = parse_expression()
        return new Assignment({target, value}, Span.combine(span, value))
    }

    function parse_field_access(target: Expression): Expression {
        while (tokens.simple_peek() === ".") {
            const span = tokens.consume().span
            const field_token = tokens.consume()
            let field: string | number
            if (field_token instanceof Identifier) {
                field = field_token.value
            } else if (field_token instanceof NumberToken) {
                field = parseInt(field_token.value)
            } else {
                throw new ParseError(`Expected identifier or number`, field_token.span)
            }
            target = new FieldAccess({target, field}, Span.combine(span, field_token))
        }
        return target
    }

    function parse_extern_block(): ExternBlock {
        const span = tokens.consume().span
        tokens.expect(":")
        const res = new ExternBlock(span)
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            const type = tokens.simple_peek()
            if (type === "fn") {
                const declaration = parse_function_declaration()
                res.functions.push(declaration)
            } else if (type === "struct") {
                const struct = parse_struct_definition()
                res.structs.push(struct)
            } else if (type === "impl") {
                const impl = parse_impl("declarations_only")
                res.impls.push(impl)
            } else {
                throw new ParseError(`Unknown ${quote("extern")} type ${quote(type)}`, span)
            }
        }
        const end_span = tokens.expect("end").span
        res.span = Span.combine(span, end_span)
        return res
    }

    function parse_impl(mode: "declarations_only" | "all"): ImplDefinition {
        const span = tokens.consume().span
        const name_or_trait = tokens.expect_identifier().value
        let type_parameters = try_parse_generic_type_parameters()
        let trait_name: string | undefined
        let target_struct_name = name_or_trait
        if (tokens.simple_peek() === "for") {
            tokens.consume()
            trait_name = name_or_trait
            target_struct_name = tokens.expect_identifier().value
            type_parameters = try_parse_generic_type_parameters()
        }
        tokens.expect(":")
        const member_functions: FunctionDefinition[] = []
        const static_functions: FunctionDefinition[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            let fn: FunctionDefinition
            if (mode === "declarations_only") {
                const declaration = parse_function_declaration()
                fn = new FunctionDefinition(
                    {declaration, block: new Block({body: []}, declaration.span)},
                    declaration.span,
                )
            } else {
                fn = parse_function_definition()
            }
            if (fn.declaration.parameters[0]?.name !== "self") {
                static_functions.push(fn)
            } else {
                member_functions.push(fn)
            }
        }
        const end_span = tokens.expect("end").span
        const impl = new ImplDefinition(
            {
                target_name: target_struct_name,
                type_parameters,
                trait_name,
                member_functions,
                static_functions,
            },
            Span.combine(span, end_span),
        )
        return impl
    }

    function parse_trait_definition(): TraitDeclaration {
        const span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const type_parameters = try_parse_generic_type_parameters()
        tokens.expect(":")
        const trait = new TraitDeclaration(
            {
                name,
                member_function_declarations: [],
                static_function_declarations: [],
                member_function_default_impls: {},
                static_function_default_impls: {},
                type_parameters,
            },
            span,
        )
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            const declaration = parse_function_declaration()
            if (![":", "=>"].includes(tokens.simple_peek() ?? "")) {
                // Ok, this is a declaration only.
                if (declaration.parameters[0]?.name !== "self") {
                    trait.static_function_declarations.push(declaration)
                } else {
                    trait.member_function_declarations.push(declaration)
                }
            } else {
                const fn = parse_function_body(declaration)
                if (fn.declaration.parameters[0].name !== "self") {
                    trait.static_function_default_impls[fn.declaration.name] = fn
                } else {
                    trait.member_function_default_impls[fn.declaration.name] = fn
                }
            }
        }
        const end_span = tokens.expect("end").span
        trait.span = Span.combine(span, end_span)
        return trait
    }

    function parse_enum_declaration(): EnumDeclaration {
        let span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const type_parameters = try_parse_generic_type_parameters()
        tokens.expect(":")
        const variants: Record<string, EnumVariant> = {}
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            const name = tokens.expect_identifier().value
            let values = new TupleDeclaration({members: []}, span)
            if (tokens.simple_peek() === "(") {
                values = parse_tuple_declaration().resolved as TupleDeclaration
            }
            variants[name] = new EnumVariant({name, values}, span)
        }
        span = Span.combine(span, tokens.expect("end").span)
        return new EnumDeclaration({name, variants, type_parameters}, span)
    }

    function parse_struct_definition(): StructDeclaration {
        let span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const members: Record<string, Type> = {}
        const type_parameters = try_parse_generic_type_parameters()
        tokens.expect(":")
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            const name = tokens.expect_identifier().value
            const type = parse_type()
            members[name] = type
        }
        span = Span.combine(span, tokens.expect("end").span)
        const definition = new StructDeclaration({name, members, type_parameters}, span)
        return definition
    }

    function parse_block(mode: "normal" | "if_else") {
        const block_type = tokens.consume()
        if (!(block_type instanceof LexicalToken) || !["=>", ":"].includes(block_type.value)) {
            throw new ParseError(
                `Expected start of a block (':' or '=>') but got ${quote(block_type.toString())}`,
                block_type.span,
            )
        }
        const block: Block = new Block({body: []}, block_type.span)
        if (block_type.value === "=>") {
            block.body.push(parse_expression())
        } else {
            const end_tokens = ["end"]
            if (mode === "if_else") {
                end_tokens.push("else")
            }
            while (!tokens.at_end() && !end_tokens.includes(tokens.simple_peek() ?? "")) {
                block.body.push(parse_expression())
            }
            if (mode === "normal") {
                tokens.expect("end")
            }
        }
        return block
    }

    function parse_parenthesized_expression_or_tuple(): Expression {
        let span = tokens.consume().span
        const expression = parse_expression()
        if (tokens.simple_peek() === ",") {
            const values = [expression]
            while (!tokens.at_end() && tokens.simple_peek() !== ")") {
                tokens.expect(",")
                values.push(parse_expression())
            }
            span = Span.combine(tokens.expect(")").span, span)
            return new Tuple({values}, span)
        }
        span = Span.combine(tokens.expect(")").span, span)
        return new ParenthesizedExpression({expression}, span)
    }

    function parse_match_expression(): Match {
        const span = tokens.consume().span
        const value = parse_expression()
        const arms: MatchArm[] = []
        let wildcard_block: Block | undefined
        tokens.expect(":")
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            if (tokens.simple_peek() === "else") {
                tokens.consume()
                wildcard_block = parse_block("normal")
                // Wildcard arm is always last.
                break
            }
            const pattern = parse_match_pattern()
            const block = parse_block("normal")
            arms.push(new MatchArm({pattern, block}, Span.combine(pattern, block.span)))
        }
        const end_span = tokens.expect("end").span
        return new Match({value, arms, wildcard_block}, Span.combine(span, end_span))
    }

    function parse_match_pattern(): MatchPattern {
        const token = tokens.peek()
        if (token instanceof NumberToken) {
            tokens.consume()
            return new LiteralMatchPattern({value: parseInt(token.value)}, token.span)
        } else if (token instanceof LexicalToken && ["true", "false"].includes(token.value)) {
            tokens.consume()
            return new LiteralMatchPattern({value: token.value === "true"}, token.span)
        } else if (token instanceof LexicalToken && token.value === "_") {
            tokens.consume()
            return new WildcardMatchPattern(token.span)
        } else if (token instanceof Identifier) {
            let type_expression: IdentifierReference | FieldAccess =
                parse_identifier_reference(token)
            tokens.consume()
            if (tokens.simple_peek() === ".") {
                tokens.consume()
                const field = tokens.expect_identifier().value
                type_expression = new FieldAccess({target: type_expression, field}, token.span)
            }
            if (tokens.simple_peek() === "(") {
                // Tuples.
                const values: MatchPattern[] = []
                tokens.consume()
                while (!tokens.at_end() && tokens.simple_peek() !== ")") {
                    if (values.length > 0) {
                        tokens.expect(",")
                    }
                    values.push(parse_match_pattern())
                }
                const end_span = tokens.expect(")").span
                return new TupleMatchPattern(
                    {type_expression, values},
                    Span.combine(token.span, end_span),
                )
            }
            const fields: Record<string, MatchPattern> = {}
            let end_span = token.span
            if (tokens.simple_peek() === "{") {
                tokens.consume()
                while (!tokens.at_end() && tokens.simple_peek() !== "}") {
                    if (Object.keys(fields).length > 0) {
                        tokens.expect(",")
                    }
                    const name = tokens.expect_identifier().value
                    if (tokens.simple_peek() === ":") {
                        tokens.expect(":")
                        const value = parse_match_pattern()
                        fields[name] = value
                    } else {
                        fields[name] = new CaptureMatchPattern({name}, token.span)
                    }
                }
                end_span = tokens.expect("}").span
            }
            return new StructuredMatchPattern(
                {type_expression, fields},
                Span.combine(token.span, end_span),
            )
        } else {
            throw new ParseError(`Unexpected token ${quote(token)}`, token?.span)
        }
    }

    function parse_loop(): Loop {
        const span = tokens.expect("loop").span
        const block = parse_block("normal")
        return new Loop({block}, Span.combine(span, block.span))
    }

    function parse_if(): If {
        const span = tokens.expect("if").span
        const condition = parse_expression()
        const if_ = new If(
            {condition, then_block: parse_block("if_else")},
            Span.combine(span, condition.span),
        )
        if (tokens.simple_peek() === "else") {
            tokens.consume()
            if_.else_block = parse_block("normal")
        } else if (tokens.simple_peek() === "end") {
            tokens.consume()
        }
        return if_
    }

    function parse_variable_declaration(): VariableDeclaration {
        const mutable = tokens.simple_peek() === "mut"
        const span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const token = tokens.peek()
        let type = type_needs_to_be_inferred
        let end_span = span
        if (token instanceof Identifier) {
            type = parse_type()
            end_span = tokens.consume().span
        }
        let value: Expression | undefined
        if (tokens.simple_peek() === "=") {
            tokens.expect("=")
            value = parse_expression()
            end_span = value.span
        }
        const let_ = new VariableDeclaration(
            {name, value, type, mutable},
            Span.combine(span, end_span),
        )
        return let_
    }

    function parse_function_declaration(): FunctionDeclaration {
        const span = tokens.expect("fn").span
        const name = tokens.expect_identifier().value
        tokens.expect("(")
        const parameters: Parameter[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== ")") {
            if (parameters.length > 0) {
                tokens.expect(",")
            }
            let mutable = false
            if (tokens.simple_peek() === "mut") {
                mutable = true
                tokens.consume()
            }
            const name_token = tokens.expect_identifier()
            const type = try_parse_type() || type_needs_to_be_inferred
            parameters.push(new Parameter({name: name_token.value, type, mutable}, name_token.span))
        }
        tokens.expect(")")
        let return_type = type_needs_to_be_inferred
        if (tokens.peek() instanceof Identifier) {
            return_type = parse_type()
        }
        return new FunctionDeclaration({name, parameters, return_type}, span)
    }

    function parse_function_body(declaration: FunctionDeclaration): FunctionDefinition {
        const fn_def = new FunctionDefinition(
            {declaration, block: new Block({body: []}, declaration.span)},
            declaration.span,
        )
        fn_def.block = parse_block("normal")
        fn_def.span = Span.combine(declaration.span, fn_def.block.span)
        return fn_def
    }

    function parse_function_definition(): FunctionDefinition {
        const declaration = parse_function_declaration()
        return parse_function_body(declaration)
    }

    function parse_return_expression(): Return {
        const span = tokens.expect("return").span
        const value = parse_expression()
        return new Return({value}, Span.combine(span, value.span))
    }

    function parse_identifier_reference(token: Identifier): IdentifierReference {
        const type_arguments = try_parse_generic_type_parameters()
        const span = token.span
        return new IdentifierReference({name: token.value, type_parameters: type_arguments}, span)
    }

    function try_parse_struct_initialization(token: Identifier): StructInstantiation | undefined {
        let type_arguments: Type[] = []
        const mark = tokens.mark()
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                if (type_arguments.length > 0) {
                    tokens.expect(",")
                }
                const type: Type = parse_type()
                type_arguments.push(type)
            }
            tokens.expect(">")
        }
        if (tokens.simple_peek() !== "{") {
            tokens.reset(mark)
            return
        }
        tokens.consume()
        const args: Record<string, Expression> = {}
        while (!tokens.at_end() && tokens.simple_peek() !== "}") {
            if (Object.keys(args).length > 0) {
                tokens.expect(",")
            }
            const name = tokens.expect_identifier().value
            tokens.expect(":")
            const value = parse_expression()
            args[name] = value
        }
        const span = Span.combine(tokens.expect("}").span, token.span)
        return new StructInstantiation(
            {target_struct_name: token.value, values: args, type_arguments},
            span,
        )
    }

    function try_parse_function_call(
        target_candidate: Expression | Token,
    ): FunctionCall | undefined {
        if (tokens.simple_peek() !== "(") {
            return
        }
        tokens.consume()
        let target: Expression
        if (target_candidate instanceof Identifier) {
            target = new IdentifierReference(
                {name: target_candidate.value, type_parameters: []},
                target_candidate.span,
            )
        } else if (target_candidate instanceof Expression) {
            target = target_candidate
        } else {
            throw new ParseError(
                `Expected identifier or expression but got ${quote(target_candidate)}`,
                target_candidate.span,
            )
        }
        const args = []
        while (!tokens.at_end() && tokens.simple_peek() !== ")") {
            if (args.length > 0) {
                tokens.expect(",")
            }
            args.push(parse_expression())
        }
        const span = Span.combine(tokens.expect(")").span, target_candidate.span)
        return new FunctionCall({target, arguments: args}, span)
    }

    function try_parse_generic_type_parameters(): Type[] {
        const type_parameters: Type[] = []
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                if (type_parameters.length > 0) {
                    tokens.expect(",")
                }
                const type_parameter = parse_type()
                type_parameter.is_type_parameter = true
                type_parameters.push(type_parameter)
            }
            tokens.expect(">")
        }
        return type_parameters
    }

    function try_parse_type(): Type | undefined {
        if (tokens.peek() instanceof Identifier || tokens.simple_peek() === "(") {
            return parse_type()
        }
        return undefined
    }

    function parse_type(): Type {
        if (tokens.simple_peek() === "(") {
            return parse_tuple_declaration()
        }
        const token = tokens.expect_identifier()
        let end_span = token.span
        let name = token.value
        const type_parameters: Type[] = []
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                if (type_parameters.length > 0) {
                    tokens.expect(",")
                }
                const type_argument = parse_type()
                type_parameters.push(type_argument)
            }
            end_span = tokens.expect(">").span
        }
        return new Type({name, type_parameters}, Span.combine(token.span, end_span))
    }

    function parse_tuple_declaration(): Type {
        const span = tokens.consume().span
        const types: Type[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== ")") {
            if (types.length > 0) {
                tokens.expect(",")
            }
            const type = parse_type()
            types.push(type)
        }
        const end_span = tokens.expect(")").span
        const resolved = new TupleDeclaration({members: types}, Span.combine(span, end_span))
        const name = `(${types.map((t) => t.to_signature_string()).join(", ")})`
        return new Type({name, type_parameters: types, resolved}, Span.combine(span, end_span))
    }
}
