/**
 * This runs all tests found in `the_book.md`.
 */
import {compile, link, compile_prelude} from "./compiler"
import fs from "node:fs"
import {Span} from "./common"

// @ts-ignore
const dir = import.meta.dir
const print_code = Bun.argv.includes("--print-code")

type Test = {
    name: string
    path: string
    section: string[]
    num: number
    src: string
    span: Span
}

async function run_test(test: Test): Promise<boolean> {
    console.log(test.name)
    let pos = 0
    let expected_error: {span: Span; message: string} | null = null
    for (const line of test.src.split("\n")) {
        if (line.includes("-- Compile error:")) {
            expected_error = {
                span: new Span(pos, pos, test.span.file, test.src),
                message: line.split("-- Compile error:")[1].trim(),
            }
            break
        }
        pos += line.length + 1
    }
    const default_prelude = await compile_prelude(`${dir}/prelude_js.kl`)
    const path = `${dir}/../the_book_prelude_js.kl`
    const the_book_prelude = await compile_prelude(path, default_prelude.env)
    let compiled
    try {
        compiled = await compile({
            file: `${dir}/../the_book.md:${test.path}`,
            src: test.src,
            env: the_book_prelude.env,
            modules: new Map(),
        })
    } catch (e: any) {
        if (expected_error) {
            if (!e.message.startsWith(expected_error.message)) {
                throw new Error(
                    `Expected compile error message to start with "${expected_error.message}", got "${e.message}"`,
                )
            }
            if (e.span.pos.line !== expected_error.span.pos.line) {
                throw new Error(
                    `Expected compile error on line ${expected_error.span.pos.line}, got ${e.span.pos.line}`,
                )
            }
            return true
        }
        throw e
    }
    if (expected_error) {
        throw new Error(
            `Expected compile error at ${expected_error.span}: ${expected_error.message}`,
        )
    }
    if (print_code) {
        console.log("Code:")
        console.log(test.src)
    }
    const linked = await link({
        compiled,
        prelude:
            default_prelude.prelude +
            the_book_prelude.prelude +
            (await Bun.file(`${dir}/../the_book_prelude_js.js`).text()),
        epilogue: await Bun.file(`${dir}/../the_book_epilogue_js.js`).text(),
    })
    try {
        eval(linked)
    } catch (e: any) {
        if (e.message === "Expected panic") {
            throw new Error(`Expected panic at ${test.name}`)
        } else if (e.message === "Assertion failed") {
            throw new Error(`Assertion failed at ${test.name}`)
        }
        console.log("\nTranspiled code:")
        console.log(linked)
        console.error(`${test.path}: FAILED`, e)
        return false
    }
    return true
}

async function cli() {
    const book = fs.readFileSync(`${dir}/../the_book.md`, "utf8")

    // parse `book` into `tests` where Test.section is the list of markdown section titles that lead up to
    // the test, Test.num is the number of the test in the current section, Test.src is the
    // source code of the test, and Test.span is the span of the test in the markdown file.
    // a test starts with ```klar\n and ends with \n```
    const tests: Test[] = []
    let section: string[] = []
    let num = 0
    let src = ""
    let pos = 0
    let span: Span = new Span(0, 0, "the_book.md", book)
    for (const line of book.split("\n")) {
        pos += line.length + 1
        if (line.startsWith("##")) {
            const section_depth = line.match(/#/g)!.length
            section = section.filter((x) => x.match(/#/g)!.length < section_depth)
            section.push(line)
            num = 0
        } else if (line.startsWith("```klar")) {
            src = ""
            span = new Span(pos, 0, "the_book.md", book)
            num++
        } else if (line.startsWith("```")) {
            span.end = pos
            const path = section.map((x) => x.replaceAll("#", "").trim()).join(" > ") + ` #${num}`
            tests.push({
                section,
                num,
                src,
                span,
                name: `${path} at ${span.toString()}`,
                path,
            })
            src = ""
        } else {
            src += line + "\n"
        }
    }
    const test_pattern_index = process.argv.indexOf("--test-pattern")
    const test_pattern = test_pattern_index === -1 ? null : process.argv[test_pattern_index + 1]
    const bail = process.argv.includes("--bail")
    let overall = 0
    let failed = 0
    for (const test of tests) {
        if (test_pattern && !test.section.join(" ").match(RegExp(test_pattern, "i"))) {
            console.log("Skipping", test.path)
            continue
        }
        overall += 1
        if (!(await run_test(test))) {
            failed += 1
            if (bail) {
                break
            }
        }
    }
    if (failed) {
        throw new Error(`FAILED ${failed}/${overall} tests`)
    }
    console.log(`PASSED ${overall} tests`)
}

await cli()
