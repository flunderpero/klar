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

    get pos() {
        let line = 1
        let col = 1
        for (let i = 0; i < this.start; i++) {
            if (this.src[i] === "\n") {
                line++
                col = 1
            } else {
                col++
            }
        }
        return {line, col}
    }

    toString() {
        const {line, col} = this.pos
        return `${this.file}:${line}:${col}`
    }

    get src_text() {
        return this.src.slice(this.start, this.end)
    }

    /**
     * Get the lines of source code that this span covers.
     */
    get src_lines() {
        let x = this.start
        while (x > 0 && this.src[x - 1] !== "\n") {
            x--
        }
        let y = this.end
        while (y < this.src.length && this.src[y] !== "\n") {
            y++
        }
        return this.src.slice(x, y)
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

export function to_json(obj: any, indent = 0) {
    function break_cycles() {
        const ancestors: any = []
        return function (_: any, value: any) {
            if (typeof value !== "object" || value === null) {
                return value
            }
            if (value instanceof Span) {
                return value.toString()
            }
            // `this` is the object that value is contained in,
            // i.e., its direct parent.
            // @ts-ignore
            while (ancestors.length > 0 && ancestors.at(-1) !== this) {
                ancestors.pop()
            }
            if (ancestors.includes(value)) {
                return "[Circular]"
            }
            ancestors.push(value)
            return value
        }
    }
    return JSON.stringify(obj, break_cycles(), indent)
}
