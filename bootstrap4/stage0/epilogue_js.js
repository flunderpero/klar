let res = exported.klar_main()
if (!res) {
    process.exit(0)
}
if (res.constructor.name === "klar_Result_Error") {
    console.error(res.klar_to_str().value)
    console.error("\nError trace:\n")
    console.error(res.klar_error_return_trace().klar_to_str().value)
    process.exit(1)
}
if (res.constructor.name === "klar_Int") {
    process.exit(res.value)
}
process.exit(0)
