function klar_assert(condition) {
    if (!condition.value) {
        klar_panic(new str("Assertion failed"))
    }
}

function klar_assert_panic(f) {
    const old_panic = global.klar_panic;
    let panicked = false;
    global.klar_panic = function () {
        panicked = true;
    };
    f();
    global.klar_panic = old_panic;
    if (!panicked) {
        klar_panic(new str("Expected panic"))
    }
}
