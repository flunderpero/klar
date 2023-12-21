function klar_assert_panic(f) {
    const old_panic = global.klar_panic;
    let panicked = false;
    global.klar_panic = function () {
        panicked = true;
    };
    f();
    global.klar_panic = old_panic;
    if (!panicked) {
        klar_panic(new klar_str("Expected panic"), new klar_str(""), new klar_str(""))
    }
}
