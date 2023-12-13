// @ts-ignore
const dir = import.meta.dir
import {readdir} from "node:fs/promises"
import {join} from "node:path"

const test_dir = join(dir, "../tests/stage0")
const build_dir = join(dir, "build")

async function test_file(file: string, update = false): Promise<boolean> {
    const dst = join(build_dir, file.replace(".kl", ""))
    const compiler = Bun.spawnSync(["bun", join(dir, "compiler.ts"), join(test_dir, file), dst])
    const actual_compiler_output = compiler.stdout.toString() + compiler.stderr.toString().trim()
    let actual_exit_code = ""
    let actual_stdout = ""
    let actual_stderr = ""
    if (compiler.exitCode === 0) {
        const compiled = Bun.spawnSync(["bash", "-c", dst])
        actual_stdout = compiled.stdout.toString().trim()
        actual_stderr = compiled.stderr.toString().trim()
        actual_exit_code = `${compiled.exitCode}`
    }
    const src = await Bun.file(join(test_dir, file)).text()
    if (update) {
        console.log("Updating", file)
        const new_src = src.replace(/--- expected[\s\S]*?\/expected ---/, "").trim()
        let result = ""
        result += "--- expected\n\n"
        result += "# compiler output:\n"
        if (actual_compiler_output) {
            result += actual_compiler_output + "\n"
        }
        result += "# exit code:\n"
        if (actual_exit_code) {
            result += actual_exit_code + "\n"
        }
        result += "# stderr:\n"
        if (actual_stderr) {
            result += actual_stderr + "\n"
        }
        result += "# stdout:\n"
        if (actual_stdout) {
            result += actual_stdout + "\n"
        }
        result += "\n/expected ---\n\n"
        result += new_src
        await Bun.write(join(test_dir, file), result)
    } else {
        if (!src.includes("--- expected")) {
            console.error("No expected output in test file, use --update to create one")
            return false
        }
        const expected = src.split("---")[1].replace("/expected", "").trim()
        const expected_compiler_output = expected
            .split("# compiler output:")[1]
            ?.split("# exit code:")[0]
            ?.trim()
        const expected_exit_code = expected.split("# exit code:")[1]?.split("# stderr:")[0]?.trim()
        const expected_stderr = expected.split("# stderr:")[1]?.split("# stdout:")[0]?.trim()
        const expected_stdout = expected.split("# stdout:")[1]?.trim()
        if (expected_compiler_output !== actual_compiler_output) {
            console.error("Compiler output mismatch:")
            console.error("Expected:", expected_compiler_output)
            console.error("Got     :", actual_compiler_output)
            return false
        }
        if (expected_exit_code !== actual_exit_code) {
            console.error("Exit code mismatch:")
            console.error("Expected:", expected_exit_code)
            console.error("Got     :", actual_exit_code)
            return false
        }
        if (expected_stderr !== actual_stderr) {
            console.error("Stderr mismatch:")
            console.error("Expected:", expected_stderr)
            console.error("Got     :", actual_stderr)
            return false
        }
        if (expected_stdout !== actual_stdout) {
            console.error("Stdout mismatch:")
            console.error("Expected:", expected_stdout)
            console.error("Got     :", actual_stdout)
            return false
        }
    }
    return true
}

async function cli() {
    const update = process.argv.includes("--update")
    const files = (await readdir(test_dir)).filter((x) => x.endsWith(".kl"))
    let failed = 0
    let total = 0
    for (const file of files) {
        total += 1
        console.log("Running", file)
        if (!(await test_file(file, update))) {
            failed += 1
        }
    }
    if (failed) {
        console.error(`FAILED ${failed}/${total} tests`)
        process.exit(1)
    }
    console.log(`PASSED ${total} tests`)
}

cli()
