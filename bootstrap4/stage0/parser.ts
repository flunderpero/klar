import {HasKindAndSpan, quote, Span} from "./common"
import {
    CharToken,
    Identifier,
    InterpolatedStringPartExpression,
    InterpolatedStringPartLiteral,
    InterpolatedStringToken,
    LexicalToken,
    NumberToken,
    StringToken,
    Token,
} from "./lexer"

type Mark = number

export class TokenStream {
    index = 0

    constructor(public tokens: Token[]) {}

    at_end() {
        return this.index >= this.tokens.length
    }

    peek(ahead = 0): Token | undefined {
        return this.tokens[this.index + ahead]
    }

    consume() {
        if (this.at_end()) {
            throw new ParseError("Unexpected EOF", this.tokens[this.index - 1]?.span)
        }
        return this.tokens[this.index++]
    }

    prev_span() {
        return this.tokens[this.index - 1]?.span
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

    simple_peek(ahead = 0) {
        const token = this.peek(ahead)
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
        let message = `${error} at ${span ?? "EOF"}`
        if (span) {
            message += "\n" + span.src_lines
        }
        super(message)
    }
}

type TypeInterface = {
    name: string
    declaration?: DeclarationOrDefinition
}

type ASTNodeAttributes = {
    type?: TypeInterface
    target_type?: TypeInterface
}

export abstract class ASTNode extends HasKindAndSpan {
    kind = "ast node"

    attributes: ASTNodeAttributes = {}

    constructor(span: Span) {
        super(span)
    }

    contained_types(): TypeDeclaration[] {
        return []
    }

    contained_nodes(): ASTNode[] {
        return []
    }
}

// Type

export class TypeDeclaration extends HasKindAndSpan {
    kind = "type"
    name: string
    type_parameters: TypeDeclaration[]
    type_parameter_default?: TypeDeclaration
    trait_bounds: TypeDeclaration[] = []

    constructor(
        data: {
            name: string
            type_parameters: TypeDeclaration[]
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof TypeDeclaration.prototype)
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

    equals(other: TypeDeclaration): boolean {
        if (this.name !== other.name) {
            return false
        }
        if (this.type_parameters.length !== other.type_parameters.length) {
            return false
        }
        for (let i = 0; i < this.type_parameters.length; i++) {
            if (!this.type_parameters[i].equals(other.type_parameters[i])) {
                return false
            }
        }
        return true
    }
}

export class TupleTypeDeclaration extends TypeDeclaration {
    kind = "tuple type"
    fields: TypeDeclaration[]

    constructor(data: {fields: TypeDeclaration[]}, span: Span) {
        super({name: "", type_parameters: []}, span)
        Object.assign(this as typeof data, data as typeof TupleTypeDeclaration.prototype)
    }

    to_signature_string(): string {
        return `(${this.fields.map((t) => t.to_signature_string()).join(", ")})`
    }

    contained_types(): TypeDeclaration[] {
        return this.fields
    }
}

export class UnitTypeDeclaration extends TypeDeclaration {
    kind = "unit type"

    constructor(span: Span) {
        super({name: "", type_parameters: []}, span)
    }
}

export class FunctionTypeDeclaration extends TypeDeclaration {
    kind = "function type"
    arg_types: TypeDeclaration[]
    return_type: TypeDeclaration
    throws?: true | TypeDeclaration

    constructor(
        data: {
            arg_types: TypeDeclaration[]
            return_type: TypeDeclaration
            throws?: true | TypeDeclaration
        },
        span: Span,
    ) {
        super({name: "", type_parameters: []}, span)
        Object.assign(this as typeof data, data as typeof FunctionTypeDeclaration.prototype)
    }

    to_signature_string(): string {
        return `(${this.arg_types
            .map((t) => t.to_signature_string())
            .join(", ")}) => ${this.return_type.to_signature_string()}`
    }

    contained_types(): TypeDeclaration[] {
        return [...this.arg_types, this.return_type]
    }
}

// Declarations and definitions

export abstract class DeclarationOrDefinition extends ASTNode {
    kind = "declaration block"
}

export class FunctionDeclaration extends DeclarationOrDefinition {
    kind = "function declaration"
    name: string
    type_parameters: TypeDeclaration[]
    parameters: Parameter[]
    return_type: TypeDeclaration
    throws?: true | TypeDeclaration
    declare attributes: ASTNodeAttributes & {type?: TypeInterface & {return_type: TypeInterface}}

    constructor(
        data: {
            name: string
            type_parameters: TypeDeclaration[]
            parameters: Parameter[]
            return_type: TypeDeclaration
            throws?: true | TypeDeclaration
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionDeclaration.prototype)
    }

    to_signature_string() {
        return `${this.name}<${this.type_parameters
            .map((t) => t.to_signature_string())
            .join(", ")}>(${this.parameters
            .map((p) => p.to_signature_string())
            .join(", ")}): ${this.return_type.to_signature_string()}`
    }

    contained_types() {
        return [this.return_type, ...this.parameters.map((p) => p.type)].filter(
            (x) => !!x,
        ) as TypeDeclaration[]
    }
}

export class Parameter extends ASTNode {
    kind = "parameter"
    name: string
    type: TypeDeclaration
    mutable: boolean

    constructor(data: {name: string; type: TypeDeclaration; mutable: boolean}, span: Span) {
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

export class ClosureParameter extends ASTNode {
    kind = "closure parameter"
    name: string
    type?: TypeDeclaration
    mutable: boolean

    constructor(data: {name: string; type?: TypeDeclaration; mutable: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Parameter.prototype)
    }

    to_signature_string() {
        return `${this.name} ${this.type?.to_signature_string() ?? "<unknown>"}`
    }

    contained_types() {
        return this.type ? [this.type] : []
    }
}

export class FunctionDefinition extends DeclarationOrDefinition {
    kind = "function definition"
    declaration: FunctionDeclaration
    block: Block

    constructor(
        data: {
            declaration: FunctionDeclaration
            block: Block
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

export class VariableDeclaration extends DeclarationOrDefinition {
    kind = "variable declaration"
    name: string
    value: Expression
    type?: TypeDeclaration
    mutable: boolean

    constructor(
        data: {
            name: string
            value: Expression
            type?: TypeDeclaration
            mutable: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof VariableDeclaration.prototype)
    }

    contained_types() {
        return this.type ? [this.type] : []
    }

    contained_nodes() {
        return this.value ? [this.value] : []
    }

    to_signature_string() {
        return `${this.name}: ${this.type?.to_signature_string() ?? "<unknown>"}`
    }
}

export class StructDeclaration extends DeclarationOrDefinition {
    kind = "struct declaration"
    name: string
    fields: Record<string, TypeDeclaration>
    type_parameters: TypeDeclaration[]

    constructor(
        data: {
            name: string
            fields: Record<string, TypeDeclaration>
            type_parameters: TypeDeclaration[]
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
        return Object.values(this.fields).concat(this.type_parameters)
    }
}

export class EnumDeclaration extends DeclarationOrDefinition {
    kind = "enum declaration"
    name: string
    variants: EnumVariant[]
    type_parameters: TypeDeclaration[]

    constructor(
        data: {
            name: string
            variants: EnumVariant[]
            type_parameters: TypeDeclaration[]
            extern?: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof EnumDeclaration.prototype)
    }

    get_variant(name: string): EnumVariant | undefined {
        return this.variants.find((v) => v.name === name)
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

export class EnumVariant extends HasKindAndSpan {
    kind = "enum variant"
    name: string
    fields: TupleTypeDeclaration

    constructor(data: {name: string; fields: TupleTypeDeclaration}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof EnumVariant.prototype)
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

    contained_nodes(): ASTNode[] {
        return [...this.functions, ...this.structs, ...this.impls]
    }
}

export class ImplDefinition extends DeclarationOrDefinition {
    kind = "impl_definition"
    trait_name?: string
    type_parameters: TypeDeclaration[]
    trait_type_parameters: TypeDeclaration[]
    target_name: string
    functions: (FunctionDefinition | FunctionDeclaration)[]
    attributes: ASTNodeAttributes & {trait_declaration?: TraitDeclaration} = {}

    constructor(
        data: {
            trait_name?: string
            trait_type_parameters: TypeDeclaration[]
            type_parameters: TypeDeclaration[]
            target_name: string
            functions: (FunctionDefinition | FunctionDeclaration)[]
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof ImplDefinition.prototype)
    }

    contained_nodes(): ASTNode[] {
        return this.functions
    }

    contained_types(): TypeDeclaration[] {
        return this.type_parameters
    }
}

export class TraitDeclaration extends DeclarationOrDefinition {
    kind = "trait declaration"
    name: string
    functions: (FunctionDeclaration | FunctionDefinition)[]
    type_parameters: TypeDeclaration[]
    trait_bounds: TypeDeclaration[]

    constructor(
        data: {
            name: string
            functions: (FunctionDeclaration | FunctionDefinition)[]
            type_parameters: TypeDeclaration[]
            trait_bounds: TypeDeclaration[]
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
        return this.functions
    }
}

// Statements

export abstract class Statement extends ASTNode {
    kind = "statement"
    constructor(span: Span) {
        super(span)
    }
}

export class UnitOperator extends Statement {
    kind = "unit operator"
    expression: Expression

    constructor(data: {expression: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof UnitOperator.prototype)
    }

    contained_nodes() {
        return [this.expression]
    }
}

export class Return extends Statement {
    kind = "return"
    value: Expression

    constructor(data: {value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Return.prototype)
    }

    contained_nodes(): ASTNode[] {
        return this.value ? [this.value] : []
    }
}

export class Break extends Statement {
    kind = "break"

    constructor(span: Span) {
        super(span)
    }
}

export class Continue extends Statement {
    kind = "continue"

    constructor(span: Span) {
        super(span)
    }
}

export class Use extends DeclarationOrDefinition {
    kind = "use"
    path: string[]
    attributes: ASTNodeAttributes & {enum_variants?: EnumVariant[]} = {}

    constructor(data: {path: string[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Use.prototype)
    }
}

// Expressions

export abstract class Expression extends ASTNode {
    kind = "expression"
    constructor(span: Span) {
        super(span)
    }
}

export class TupleInstantiation extends Expression {
    kind = "tuple"
    elements: Expression[]

    constructor(data: {elements: Expression[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof TupleInstantiation.prototype)
    }

    contained_nodes() {
        return this.elements
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

export class Number_ extends Expression {
    kind = "number"
    value: string

    constructor(data: {value: string}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Number_.prototype)
    }
}

export class UnitLiteral extends Expression {
    kind = "unit"

    constructor(span: Span) {
        super(span)
    }
}

export class Str extends Expression {
    kind = "string"
    value: string
    is_multiline: boolean

    constructor(data: {value: string; is_multiline: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Str.prototype)
    }
}

export class Char extends Expression {
    kind = "Char"
    value: string

    constructor(data: {value: string}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Char.prototype)
    }
}

export class InterpolatedStr extends Expression {
    kind = "interpolated string"
    expressions: Expression[]
    is_multiline: boolean

    constructor(data: {expressions: Expression[]; is_multiline: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof InterpolatedStr.prototype)
    }

    contained_nodes() {
        return this.expressions
    }
}

export class Bool extends Expression {
    kind = "Bool"
    value: boolean

    constructor(data: {value: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Bool.prototype)
    }
}

export class Not extends Expression {
    kind = "not"
    expression: Expression

    constructor(data: {expression: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Not.prototype)
    }

    contained_nodes(): ASTNode[] {
        return [this.expression]
    }
}

export class FunctionCall extends Expression {
    kind = "function call"
    target: Expression
    type_arguments: TypeDeclaration[]
    args: Expression[]
    propagate_error: boolean

    constructor(
        data: {
            target: Expression
            args: Expression[]
            type_arguments: TypeDeclaration[]
            propagate_error: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionCall.prototype)
    }

    contained_nodes() {
        return [this.target, ...this.args]
    }
}

export class StructInstantiation extends Expression {
    kind = "struct instantiation"
    type_arguments: TypeDeclaration[]
    target_struct_name: string
    fields: Record<string, Expression | undefined>

    constructor(
        data: {
            type_arguments: TypeDeclaration[]
            target_struct_name: string
            fields: Record<string, Expression | undefined>
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
        return Object.values(this.fields).filter((x) => !!x) as Expression[]
    }
}

export class IdentifierReference extends Expression {
    kind = "identifier reference"
    name: string
    type_parameters: TypeDeclaration[]

    constructor(data: {name: string; type_parameters: TypeDeclaration[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof IdentifierReference.prototype)
    }

    contained_types() {
        return this.type_parameters
    }
}

export class Assignment extends Statement {
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

export class FQN extends Expression {
    kind = "fqn"
    parts: IdentifierReference[]

    constructor(data: {parts: IdentifierReference[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FQN.prototype)
    }

    contained_nodes() {
        return this.parts
    }
}

export class FieldAccess extends Expression {
    kind = "field access"
    target: Expression
    field: string
    field_type_arguments: TypeDeclaration[]

    constructor(
        data: {target: Expression; field: string; field_type_arguments: TypeDeclaration[]},
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof FieldAccess.prototype)
    }

    contained_nodes() {
        return [this.target]
    }
}

export class IndexedAccess extends Expression {
    kind = "indexed access"
    target: Expression
    index: Expression
    is_write: boolean

    constructor(data: {target: Expression; index: Expression; is_write: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof IndexedAccess.prototype)
    }

    contained_nodes() {
        return [this.target, this.index]
    }
}

export class ArrayLiteral extends Expression {
    kind = "array literal"
    elements: Expression[]
    name = ""

    constructor(data: {elements: Expression[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof ArrayLiteral.prototype)
    }

    contained_nodes() {
        return this.elements
    }

    to_signature_string() {
        return `[ArrayLiteral]`
    }
}

type BinaryOperator = "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "and" | "or"

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
        "%": 6,
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

export class IfLet extends Expression {
    kind = "if let"
    pattern: MatchPattern
    value: Expression
    then_block: Block
    else_block?: Block

    constructor(
        data: {pattern: MatchPattern; value: Expression; then_block: Block; else_block?: Block},
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof IfLet.prototype)
    }

    contained_nodes() {
        return [this.value, this.then_block, ...(this.else_block ? [this.else_block] : [])]
    }
}

export class Match extends Expression {
    kind = "match"
    value: Expression
    arms: MatchArm[]
    must_exhaust: boolean

    constructor(data: {value: Expression; arms: MatchArm[]; must_exhaust: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Match.prototype)
    }

    contained_nodes() {
        return [this.value, ...this.arms]
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

export class AlternativeMatchPattern extends MatchPattern {
    kind = "alternative match pattern"
    patterns: MatchPattern[]

    constructor(data: {patterns: MatchPattern[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof AlternativeMatchPattern.prototype)
    }

    contained_nodes() {
        return this.patterns
    }
}

export class LiteralMatchPattern extends MatchPattern {
    kind = "literal number match pattern"
    value: Number_ | Bool | Char | Str

    constructor(data: {value: Number_ | Bool | Char | Str}, span: Span) {
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

export class RangeMatchPattern extends MatchPattern {
    kind = "range match pattern"
    start: Number_ | Char
    end: Number_ | Char
    is_closed: boolean

    constructor(
        data: {start: Number_ | Char; end: Number_ | Char; is_closed: boolean},
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof RangeMatchPattern.prototype)
    }
}

export class CaptureMatchPatternOrType extends MatchPattern {
    kind = "capture match pattern"
    name: string

    constructor(data: {name: string}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof CaptureMatchPatternOrType.prototype)
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

export class Loop extends Statement {
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

export class Block extends Expression {
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

export class ClosureDefinition extends Expression {
    kind = "closure definition"
    parameters: ClosureParameter[]
    return_type: TypeDeclaration
    block: Block
    throws?: true | TypeDeclaration
    declare attributes: ASTNodeAttributes & {type?: TypeInterface & {return_type: TypeInterface}}

    constructor(
        data: {
            parameters: ClosureParameter[]
            return_type: TypeDeclaration
            block: Block
            throws?: true | TypeDeclaration
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof ClosureDefinition.prototype)
    }

    contained_nodes() {
        return [this.block]
    }

    contained_types() {
        return [this.return_type, ...this.parameters.map((p) => p.type)].filter(
            (x) => !!x,
        ) as TypeDeclaration[]
    }
}

const self_type = new TypeDeclaration(
    {name: "Self", type_parameters: []},
    new Span(0, 0, "<builtin>", ""),
)

export const unit_type = new TypeDeclaration(
    {name: "()", type_parameters: []},
    new Span(0, 0, "<builtin>", ""),
)

export type AST = Block

/**
 * Parse tokens into an AST.
 *
 * FEAT: The syntax is correct.
 */
export function parse(tokens: TokenStream): AST {
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
        const expression = parse_binary_expression(1)
        if (tokens.simple_peek() === ";") {
            tokens.consume()
            return new UnitOperator({expression}, expression.span)
        }
        return expression
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
                if (["(", "<"].includes(tokens.simple_peek(2)!)) {
                    expression = parse_function_definition()
                } else {
                    expression = parse_closure()
                }
            } else if (simple_token === "let" || simple_token === "mut") {
                expression = parse_variable_declaration()
            } else if (simple_token === "true" || simple_token === "false") {
                const span = tokens.consume().span
                expression = new Bool({value: simple_token === "true"}, span)
            } else if (simple_token === "not") {
                const span = tokens.consume().span
                const value = parse_primary()
                expression = new Not({expression: value}, Span.combine(span, value))
            } else if (simple_token === "if") {
                expression = parse_if_or_if_let()
            } else if (simple_token === "loop") {
                expression = parse_loop()
            } else if (simple_token === "(") {
                expression = parse_parenthesized_expression_or_tuple_or_unit_type()
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
            } else if (simple_token === "[") {
                expression = parse_array_literal()
            } else if (simple_token === "use") {
                expression = parse_use()
            }
        } else if (token instanceof NumberToken) {
            tokens.consume()
            expression = new Number_({value: token.value}, token.span)
        } else if (token instanceof StringToken) {
            tokens.consume()
            expression = new Str({value: token.value, is_multiline: token.is_multiline}, token.span)
        } else if (token instanceof CharToken) {
            tokens.consume()
            expression = new Char({value: token.value}, token.span)
        } else if (token instanceof InterpolatedStringToken) {
            tokens.consume()
            expression = parse_interpolated_string(token)
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
            } else if (tokens.simple_peek() === "[") {
                // FIXME: The grammar is ambiguous here. This could be a standalone array literal.
                expression = parse_indexed_access(expression)
                // } else if (tokens.simple_peek() === ";") {
                //     tokens.consume()
                //     if (expression instanceof Statement) {
                //         throw new ParseError(`Expected expression but got statement`, expression.span)
                //     }
                //     expression = new UnitOperator({expression}, expression.span)
                //     break
            } else {
                break
            }
        }
        return expression
    }

    function parse_use(): Use {
        let span = tokens.consume().span
        let end_span = span
        const path = []
        if (tokens.simple_peek() === ".") {
            end_span = tokens.expect(".").span
            path.push(".")
        }
        while (true) {
            const token = tokens.consume()
            if (
                !(
                    token instanceof Identifier ||
                    (token instanceof LexicalToken && token.value === "*")
                )
            ) {
                throw new ParseError(`Expected identifier or *`, token.span)
            }
            end_span = token.span
            path.push(token.value)
            if (tokens.simple_peek() !== "::") {
                break
            }
            end_span = tokens.consume().span
        }
        return new Use({path}, Span.combine(span, end_span))
    }

    function parse_assignment(target: Expression): Assignment {
        const span = tokens.consume().span
        const value = parse_expression()
        if (target instanceof IndexedAccess) {
            target.is_write = true
        }
        return new Assignment({target, value}, Span.combine(span, value))
    }

    function parse_field_access(target: Expression): Expression {
        while (tokens.simple_peek() === ".") {
            tokens.consume()
            const field_token = tokens.consume()
            let field: string | number
            if (field_token instanceof Identifier || field_token instanceof NumberToken) {
                field = field_token.value
            } else {
                throw new ParseError(`Expected identifier or number`, field_token.span)
            }
            const field_type_arguments = try_parse_generic_type_parameters()
            target = new FieldAccess(
                {target, field, field_type_arguments},
                Span.combine(target.span, field_token),
            )
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
                const impl = parse_impl("extern")
                res.impls.push(impl)
            } else {
                throw new ParseError(`Unknown ${quote("extern")} type ${quote(type)}`, span)
            }
        }
        const end_span = tokens.expect("end").span
        res.span = Span.combine(span, end_span)
        return res
    }

    function parse_impl(mode: "extern" | "all"): ImplDefinition {
        const span = tokens.consume().span
        const name_or_trait = tokens.expect_identifier().value
        let type_parameters = try_parse_generic_type_parameters()
        let trait_type_parameters: TypeDeclaration[] = []
        let trait_name: string | undefined
        let target_struct_name = name_or_trait
        if (tokens.simple_peek() === "for") {
            tokens.consume()
            trait_name = name_or_trait
            target_struct_name = tokens.expect_identifier().value
            trait_type_parameters = type_parameters
            type_parameters = try_parse_generic_type_parameters()
        }
        if (mode === "extern" && trait_name) {
            // There must be no function declarations when implementing a trait
            // in an extern block.
            if (tokens.simple_peek() === ":") {
                throw new ParseError(
                    `Trait implementations in extern blocks must not have function definitions`,
                    tokens.peek()!.span,
                )
            }
            return new ImplDefinition(
                {
                    trait_name,
                    trait_type_parameters,
                    target_name: target_struct_name,
                    type_parameters,
                    functions: [],
                },
                span,
            )
        }
        tokens.expect(":")
        const functions: (FunctionDefinition | FunctionDeclaration)[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            let fn: FunctionDefinition | FunctionDeclaration
            if (mode === "extern") {
                const declaration = parse_function_declaration()
                fn = declaration
            } else {
                fn = parse_function_definition()
            }
            functions.push(fn)
        }
        const end_span = tokens.expect("end").span
        const impl = new ImplDefinition(
            {
                target_name: target_struct_name,
                type_parameters,
                trait_name,
                trait_type_parameters,
                functions,
            },
            Span.combine(span, end_span),
        )
        return impl
    }

    function parse_trait_definition(): TraitDeclaration {
        const span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const trait_bounds = []
        if (tokens.simple_peek() === "impl") {
            tokens.consume()
            trait_bounds.push(parse_type())
            while (tokens.simple_peek() === "and") {
                tokens.consume()
                trait_bounds.push(parse_type())
            }
        }
        const type_parameters = try_parse_generic_type_parameters()
        tokens.expect(":")
        const functions: (FunctionDeclaration | FunctionDefinition)[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            const declaration = parse_function_declaration()
            if (![":", "=>"].includes(tokens.simple_peek() ?? "")) {
                // Ok, this is a declaration only.
                functions.push(declaration)
            } else {
                const fn = parse_function_body(declaration)
                functions.push(fn)
            }
        }
        const end_span = tokens.expect("end").span
        return new TraitDeclaration(
            {name, functions, type_parameters, trait_bounds},
            Span.combine(span, end_span),
        )
    }

    function parse_enum_declaration(): EnumDeclaration {
        let span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const type_parameters = try_parse_generic_type_parameters()
        tokens.expect(":")
        const variants = []
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            const name = tokens.expect_identifier().value
            let fields = new TupleTypeDeclaration({fields: []}, span)
            if (tokens.simple_peek() === "(") {
                const tuple_type = parse_tuple_or_unit_type()
                if (!(tuple_type instanceof TupleTypeDeclaration)) {
                    throw new ParseError(
                        `Expected tuple type but got ${quote(tuple_type.to_signature_string())}`,
                        tuple_type.span,
                    )
                }
                fields = tuple_type
            }
            variants.push(new EnumVariant({name, fields}, span))
        }
        span = Span.combine(span, tokens.expect("end").span)
        return new EnumDeclaration({name, variants, type_parameters}, span)
    }

    function parse_struct_definition(): StructDeclaration {
        let span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const members: Record<string, TypeDeclaration> = {}
        const type_parameters = try_parse_generic_type_parameters()
        tokens.expect(":")
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            const name = tokens.expect_identifier().value
            const type = parse_type()
            if (members[name]) {
                throw new ParseError(`Duplicate member ${quote(name)}`, type.span)
            }
            members[name] = type
        }
        span = Span.combine(span, tokens.expect("end").span)
        return new StructDeclaration({name, fields: members, type_parameters}, span)
    }

    function parse_block() {
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
            while (!tokens.at_end() && tokens.simple_peek() !== "end") {
                block.body.push(parse_expression())
            }
            tokens.expect("end")
        }
        return block
    }

    function parse_parenthesized_expression_or_tuple_or_unit_type(): Expression {
        let span = tokens.consume().span
        if (tokens.simple_peek() === ")") {
            span = Span.combine(tokens.expect(")").span, span)
            return new UnitLiteral(span)
        }
        const expression = parse_expression()
        if (tokens.simple_peek() === ",") {
            const values = [expression]
            while (!tokens.at_end() && tokens.simple_peek() !== ")") {
                tokens.expect(",")
                values.push(parse_expression())
            }
            span = Span.combine(tokens.expect(")").span, span)
            return new TupleInstantiation({elements: values}, span)
        }
        span = Span.combine(tokens.expect(")").span, span)
        return new ParenthesizedExpression({expression}, span)
    }

    function parse_indexed_access(target: Expression): Expression {
        tokens.consume()
        const index = parse_expression()
        const end_span = tokens.expect("]").span
        // The default value of `is_write` is `false`.
        // It will be set to `true` in `parse_assignment`.
        return new IndexedAccess(
            {target, index, is_write: false},
            Span.combine(target.span, end_span),
        )
    }

    function parse_array_literal(): ArrayLiteral {
        const span = tokens.consume().span
        const values: Expression[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== "]") {
            if (values.length > 0) {
                tokens.expect(",")
            }
            values.push(parse_expression())
        }
        const end_span = tokens.expect("]").span
        return new ArrayLiteral({elements: values}, Span.combine(span, end_span))
    }

    function parse_match_expression(): Match {
        const span = tokens.consume().span
        const value = parse_expression()
        const arms: MatchArm[] = []
        tokens.expect(":")
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            const pattern = parse_match_pattern()
            const block = parse_block()
            arms.push(new MatchArm({pattern, block}, Span.combine(pattern, block.span)))
        }
        const end_span = tokens.expect("end").span
        return new Match({value, arms, must_exhaust: true}, Span.combine(span, end_span))
    }

    function parse_range(start: Number_ | Char): RangeMatchPattern {
        tokens.consume()
        let is_closed = true
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            is_closed = false
        }
        const end = tokens.consume() as any
        if (end.kind !== start.kind) {
            throw new ParseError(`Expected ${quote(start.kind)} but got ${quote(end)}`, end.span)
        }
        return new RangeMatchPattern(
            {
                start,
                end: new (start.constructor as any)({value: end.value}, end.span),
                is_closed,
            },
            Span.combine(start.span, end.span),
        )
    }

    function parse_match_pattern(): MatchPattern {
        let patterns = [parse_match_pattern_single()]
        while (tokens.simple_peek() === "|") {
            tokens.consume()
            patterns.push(parse_match_pattern_single())
        }
        if (patterns.length === 1) {
            return patterns[0]
        }
        return new AlternativeMatchPattern({patterns}, Span.combine(patterns[0], patterns.at(-1)!))
    }

    function parse_match_pattern_single(): MatchPattern {
        const token = tokens.peek()
        if (token instanceof NumberToken) {
            tokens.consume()
            if (tokens.simple_peek() === "..") {
                return parse_range(new Number_({value: token.value}, token.span))
            }
            return new LiteralMatchPattern(
                {value: new Number_({value: token.value}, token.span)},
                token.span,
            )
        } else if (token instanceof StringToken) {
            tokens.consume()
            return new LiteralMatchPattern(
                {value: new Str({value: token.value, is_multiline: false}, token.span)},
                token.span,
            )
        } else if (token instanceof CharToken) {
            tokens.consume()
            if (tokens.simple_peek() === "..") {
                return parse_range(new Char({value: token.value}, token.span))
            }
            return new LiteralMatchPattern(
                {value: new Char({value: token.value}, token.span)},
                token.span,
            )
        } else if (token instanceof LexicalToken && ["true", "false"].includes(token.value)) {
            tokens.consume()
            return new LiteralMatchPattern(
                {value: new Bool({value: token.value === "true"}, token.span)},
                token.span,
            )
        } else if (token instanceof LexicalToken && token.value === "_") {
            tokens.consume()
            return new WildcardMatchPattern(token.span)
        } else if (token instanceof LexicalToken && token.value === "(") {
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
                {type_expression: new UnitLiteral(token.span), values},
                Span.combine(token.span, end_span),
            )
        } else if (token instanceof Identifier) {
            tokens.consume()
            let type_expression: IdentifierReference | FQN | FieldAccess =
                parse_identifier_reference(token)
            while (tokens.simple_peek() === ".") {
                tokens.consume()
                const field = tokens.expect_identifier().value
                type_expression = new FieldAccess(
                    {target: type_expression, field, field_type_arguments: []},
                    token.span,
                )
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
                        fields[name] = new CaptureMatchPatternOrType({name}, token.span)
                    }
                }
                end_span = tokens.expect("}").span
            }
            if (
                type_expression instanceof IdentifierReference &&
                type_expression.type_parameters.length === 0 &&
                Object.keys(fields).length === 0
            ) {
                // This is a variable capture.
                return new CaptureMatchPatternOrType({name: type_expression.name}, token.span)
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
        const block = parse_block()
        return new Loop({block}, Span.combine(span, block.span))
    }

    function parse_if_or_if_let(): If | IfLet {
        const span = tokens.expect("if").span
        if (tokens.simple_peek() === "let") {
            tokens.consume()
            const pattern = parse_match_pattern()
            tokens.expect("=")
            const value = parse_expression()
            const blocks = parse_blocks()
            return new IfLet(
                {pattern, value, then_block: blocks[0], else_block: blocks[1]},
                Span.combine(span, blocks[2]),
            )
        } else {
            const condition = parse_expression()
            const blocks = parse_blocks()
            return new If(
                {condition, then_block: blocks[0], else_block: blocks[1]},
                Span.combine(span, blocks[2]),
            )
        }
        function parse_blocks(): [Block, Block | undefined, Span] {
            const then_block = parse_block()
            let span = then_block.span
            let else_block: Block | undefined
            if (tokens.simple_peek() === "else") {
                tokens.consume()
                else_block = parse_block()
                span = else_block.span
            }
            return [then_block, else_block, span]
        }
    }

    function parse_variable_declaration(): VariableDeclaration {
        const mutable = tokens.simple_peek() === "mut"
        const span = tokens.consume().span
        const name = tokens.expect_identifier().value
        let type: TypeDeclaration | undefined
        if (tokens.peek() instanceof Identifier) {
            type = parse_type()
        }
        tokens.expect("=")
        const value = parse_expression()
        const end_span = value.span
        return new VariableDeclaration({name, value, type, mutable}, Span.combine(span, end_span))
    }

    function parse_function_declaration(): FunctionDeclaration {
        const span = tokens.expect("fn").span
        const name = tokens.expect_identifier().value
        const type_parameters = try_parse_generic_type_parameters()
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
            let type: TypeDeclaration | undefined
            if (name_token.value === "self" && parameters.length === 0) {
                type = self_type
            } else {
                type = parse_type()
            }
            parameters.push(
                new Parameter({name: name_token.value, type: type, mutable}, name_token.span),
            )
        }
        let end_span = tokens.expect(")").span
        let return_type = unit_type
        if (tokens.peek() instanceof Identifier || tokens.simple_peek() === "(") {
            return_type = parse_type()
            end_span = return_type.span
        }
        let throws: true | TypeDeclaration | undefined = undefined
        if (tokens.simple_peek() === "throws") {
            end_span = tokens.consume().span
            if (tokens.peek() instanceof Identifier) {
                throws = parse_type()
            } else {
                throws = true
            }
        }
        return new FunctionDeclaration(
            {name, type_parameters, parameters, return_type, throws},
            Span.combine(span, end_span),
        )
    }

    function parse_function_type(): FunctionTypeDeclaration {
        const span = tokens.consume().span
        tokens.expect("(")
        const parameter_types: TypeDeclaration[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== ")") {
            if (parameter_types.length > 0) {
                tokens.expect(",")
            }
            parameter_types.push(parse_type())
        }
        tokens.expect(")")
        const return_type = try_parse_type() || unit_type
        let throws: true | TypeDeclaration | undefined = undefined
        if (tokens.simple_peek() === "throws") {
            tokens.consume()
            throws = try_parse_type() || true
        }
        return new FunctionTypeDeclaration({arg_types: parameter_types, return_type, throws}, span)
    }

    function parse_function_body(declaration: FunctionDeclaration): FunctionDefinition {
        const fn_def = new FunctionDefinition(
            {declaration, block: new Block({body: []}, declaration.span)},
            declaration.span,
        )
        fn_def.block = parse_block()
        fn_def.span = Span.combine(declaration.span, fn_def.block.span)
        return fn_def
    }

    function parse_function_definition(): FunctionDefinition {
        const declaration = parse_function_declaration()
        return parse_function_body(declaration)
    }

    function parse_closure(): ClosureDefinition {
        const span = tokens.expect("fn").span
        tokens.expect("(")
        const parameters: ClosureParameter[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== ")") {
            if (parameters.length > 0) {
                tokens.expect(",")
            }
            const name_token = tokens.expect_identifier()
            const type = try_parse_type()
            parameters.push(
                new ClosureParameter(
                    {name: name_token.value, type: type, mutable: false},
                    name_token.span,
                ),
            )
        }
        tokens.expect(")")
        const return_type = try_parse_type() || unit_type
        let throws: true | TypeDeclaration | undefined = undefined
        if (tokens.simple_peek() === "throws") {
            tokens.consume()
            if (tokens.peek() instanceof Identifier) {
                throws = parse_type()
            } else {
                throws = true
            }
        }
        const block = parse_block()
        return new ClosureDefinition(
            {parameters, return_type, block, throws},
            Span.combine(span, block.span),
        )
    }

    function parse_return_expression(): Return {
        const span = tokens.expect("return").span
        const value = parse_expression()
        return new Return({value}, Span.combine(span, value.span))
    }

    function parse_identifier_reference(token: Identifier): IdentifierReference | FQN {
        let type_parameters: TypeDeclaration[]
        // TODO: We should try to make this work without backtracking.
        //       For now we have to because we may parse a less-than operator
        //       as a type argument list. We will try to parse `a < b` as a type argument
        //       list, but we should only parse `a` as an identifier reference.
        const mark = tokens.mark()
        try {
            type_parameters = try_parse_generic_type_parameters()
        } catch (e) {
            tokens.reset(mark)
            type_parameters = []
        }
        const parts = [
            new IdentifierReference(
                {name: token.value, type_parameters},
                Span.combine(token.span, tokens.prev_span()),
            ),
        ]
        while (tokens.simple_peek() === "::") {
            tokens.consume()
            const token = tokens.expect_identifier()
            const mark = tokens.mark()
            try {
                type_parameters = try_parse_generic_type_parameters()
            } catch (e) {
                tokens.reset(mark)
                type_parameters = []
            }
            parts.push(
                new IdentifierReference(
                    {name: token.value, type_parameters},
                    Span.combine(token.span, tokens.prev_span()),
                ),
            )
        }
        if (parts.length === 1) {
            return parts[0]
        }
        return new FQN({parts}, Span.combine(parts[0], parts.at(-1)!))
    }

    function try_parse_struct_initialization(token: Identifier): StructInstantiation | undefined {
        // TODO: We should try to make this work without backtracking.
        //       For now we have to because we may parse a less-than operator.
        const mark = tokens.mark()
        try {
            let type_arguments: TypeDeclaration[] = []
            const mark = tokens.mark()
            if (tokens.simple_peek() === "<") {
                tokens.consume()
                while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                    if (type_arguments.length > 0) {
                        tokens.expect(",")
                    }
                    const type: TypeDeclaration = parse_type()
                    type_arguments.push(type)
                }
                tokens.expect(">")
            }
            if (tokens.simple_peek() !== "{") {
                tokens.reset(mark)
                return
            }
            tokens.consume()
            const args: Record<string, Expression | undefined> = {}
            while (!tokens.at_end() && tokens.simple_peek() !== "}") {
                if (Object.keys(args).length > 0) {
                    tokens.expect(",")
                }
                const name = tokens.expect_identifier().value
                let value: Expression | undefined
                if (tokens.simple_peek() === ":") {
                    tokens.expect(":")
                    value = parse_expression()
                }
                args[name] = value
            }
            const span = Span.combine(tokens.expect("}").span, token.span)
            return new StructInstantiation(
                {target_struct_name: token.value, fields: args, type_arguments},
                span,
            )
        } catch (e) {
            tokens.reset(mark)
            return undefined
        }
    }

    function try_parse_function_call(
        target_candidate: Expression | Token,
    ): FunctionCall | undefined {
        const mark = tokens.mark()
        let type_arguments = try_parse_generic_type_parameters()
        if (tokens.simple_peek() !== "(") {
            tokens.reset(mark)
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
            if (target instanceof FieldAccess) {
                type_arguments = target.field_type_arguments
            } else if (target instanceof FQN) {
                type_arguments = target.parts.at(-1)!.type_parameters
            }
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
        const span = target.span
        let end_span = tokens.expect(")").span
        let propagate_error = false
        if (tokens.simple_peek() === "!") {
            propagate_error = true
            end_span = tokens.consume().span
        }
        return new FunctionCall(
            {target, args, type_arguments, propagate_error},
            Span.combine(span, end_span),
        )
    }

    function try_parse_generic_type_parameters(): TypeDeclaration[] {
        const mark = tokens.mark()
        const type_parameters: TypeDeclaration[] = []
        if (tokens.simple_peek() === "<") {
            try {
                tokens.consume()
                while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                    if (type_parameters.length > 0) {
                        tokens.expect(",")
                    }
                    const type_parameter = try_parse_type()
                    if (!type_parameter) {
                        tokens.reset(mark)
                        return []
                    }
                    if (tokens.simple_peek() === "impl") {
                        tokens.consume()
                        type_parameter.trait_bounds.push(parse_type())
                        while (tokens.simple_peek() === "and") {
                            tokens.consume()
                            type_parameter.trait_bounds.push(parse_type())
                        }
                    }
                    if (tokens.simple_peek() === "=") {
                        tokens.consume()
                        type_parameter.type_parameter_default = parse_type()
                    }
                    type_parameters.push(type_parameter)
                }
            } catch (e) {
                tokens.reset(mark)
                return []
            }
            tokens.expect(">")
        }
        return type_parameters
    }

    function try_parse_type(): TypeDeclaration | undefined {
        if (
            tokens.peek() instanceof Identifier ||
            tokens.simple_peek() === "(" ||
            tokens.simple_peek() === "fn"
        ) {
            return parse_type()
        }
        return undefined
    }

    function parse_type(): TypeDeclaration {
        if (tokens.simple_peek() === "(") {
            if (tokens.simple_peek(1) === "fn") {
                tokens.consume()
                const fn_type = parse_function_type()
                tokens.expect(")")
                return fn_type
            }
            return parse_tuple_or_unit_type()
        }
        const token = tokens.expect_identifier()
        let end_span = token.span
        let name = token.value
        const type_parameters: TypeDeclaration[] = []
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
        let type = new TypeDeclaration(
            {name, type_parameters: type_parameters},
            Span.combine(token.span, end_span),
        )
        if (tokens.simple_peek() === "?") {
            end_span = tokens.consume().span
            type = new TypeDeclaration(
                {name: "Option", type_parameters: [type]},
                Span.combine(token.span, end_span),
            )
        }
        return type
    }

    function parse_tuple_or_unit_type():
        | TupleTypeDeclaration
        | UnitTypeDeclaration
        | TypeDeclaration {
        const span = tokens.consume().span
        const fields: TypeDeclaration[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== ")") {
            if (fields.length > 0) {
                tokens.expect(",")
            }
            const type = parse_type()
            fields.push(type)
        }
        let end_span = tokens.expect(")").span
        if (fields.length === 0) {
            return new UnitTypeDeclaration(Span.combine(span, end_span))
        }
        let type: TypeDeclaration | TupleTypeDeclaration = new TupleTypeDeclaration(
            {fields},
            Span.combine(span, end_span),
        )
        if (tokens.simple_peek() === "?") {
            end_span = tokens.consume().span
            type = new TypeDeclaration(
                {name: "Option", type_parameters: [type]},
                Span.combine(span, end_span),
            )
        }
        return type
    }

    function parse_interpolated_string(token: InterpolatedStringToken): InterpolatedStr {
        let span = token.span
        const expressions: Expression[] = []
        for (const part of token.parts) {
            if (part instanceof InterpolatedStringPartLiteral) {
                expressions.push(
                    new Str({value: part.value, is_multiline: part.is_multiline}, span),
                )
            } else if (part instanceof InterpolatedStringPartExpression) {
                const ast = parse(new TokenStream(part.tokens))
                if (ast.body.length !== 1) {
                    throw new ParseError(
                        `Expected exactly one expression but got ${ast.body.length}`,
                        span,
                    )
                }
                expressions.push(ast.body[0])
            } else {
                throw new ParseError(`Unknown interpolated string part: ${part}`, part.span)
            }
        }
        if (expressions.length > 0) {
            span = Span.combine(expressions[0].span, expressions.at(-1)!.span)
        }
        return new InterpolatedStr({expressions, is_multiline: token.is_multiline}, span)
    }
}
