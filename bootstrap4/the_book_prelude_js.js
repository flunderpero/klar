function panic(message) {
    throw new Error(message)
}

function assert(condition) {
    if (!condition.value) {
        panic("Assertion failed")
    }
}

function assert_panic(f) {
    const old_panic = global.panic;
    let panicked = false;
    global.panic = function () {
        panicked = true;
    };
    f();
    global.panic = old_panic;
    if (!panicked) {
        panic("Expected panic")
    }
}
