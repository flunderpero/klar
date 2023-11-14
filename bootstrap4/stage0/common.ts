export class Span {
    static combine(a: Span | HasKindAndSpan, b: Span | HasKindAndSpan) {
        if (a instanceof HasKindAndSpan) {
            a = a.span
        }
        if (b instanceof HasKindAndSpan) {
            b = b.span
        }
        if (a.file !== b.file) {
            throw new Error("Cannot combine spans from different files")
        }
        return new Span(a.start, b.end, a.file, a.src)
    }

    constructor(
        public start: number,
        public end: number,
        public file: string,
        private src: string,
    ) {
        Object.defineProperty(this, "src", {enumerable: false})
    }

    toString() {
        let row = 1
        let col = 1
        for (let i = 0; i < this.start; i++) {
            if (this.src[i] === "\n") {
                row++
                col = 1
            } else {
                col++
            }
        }
        return `${this.file}:${row}:${col}`
    }
}

export class HasKindAndSpan {
    kind = "node"
    constructor(public span: Span) {}

    toString() {
        return `${this.kind} at ${this.span}`
    }
}

export function quote(s: any) {
    return `\`${s}\``
}
