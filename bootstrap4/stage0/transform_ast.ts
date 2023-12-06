/**
 * Transform the AST to a simpler form with with these transformations:
 *
 * - `FieldAccess.FunctionCall` is transformed to `FunctionCall(field_access, args)`
 * - `IndexedAccess.FunctionCall` is transformed to `FunctionCall(indexed_access, args)`
 */

import assert from "node:assert"
import {Span} from "./common"
import {lexer} from "./lexer"
import * as ast from "./parser"

export class ASTNode {
    constructor(public span: Span) {}

    abstract toString(): string
}

export class AST extends ASTNode {
    constructor(
        public nodes: ASTNode[],
        span: Span,
    ) {
        super(span)
    }

    toString() {
        return this.nodes.map((node) => node.toString()).join("\n")
    }
}

export class FunctionCall extends ASTNode {
    constructor(
        public target: ASTNode,
        public args: ASTNode[],
        span: Span,
    ) {
        super(span)
    }

    toString() {
        return `${this.target}(${this.args.join(", ")})`
    }
}

export function transform_ast(ast: ast.AST): AST {
    return new AST(
        ast.body.map((node) => transform(node, ast)),
        ast.span,
    )
}

function transform(node: ASTNode, parent: ASTNode): ASTNode {
    if (node instanceof ast.FunctionCall) {
        const target = transform(node.target, node)
        if (target instanceof ast.FieldAccess) {
            node.args.unshift(target.target)
        } else if (target instanceof ast.IndexedAccess) {
            node.args.unshift(target.target)
        }
        return new FunctionCall(
            target,
            node.args.map((arg) => transform(arg, node)),
            node.span,
        )
    }
    return node
}

const  tests = {
    test_transform_function_call_with_field_access() {
        const src_ast = ast.parse(new ast.TokenStream(lexer({file: "test", src: "a.b()"})))
        const final_ast = transform_ast(src_ast)
        assert.equal(final_ast.toString(), "b(a)")
    }
}

if (Bun.argv[1].endsWith("transform_ast.ts")) {
    for (const [name, fn] of Object.entries(tests)) {
        console.log(name)
        fn()
    }
}

