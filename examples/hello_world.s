# This is what `hello-world.kl` should roughly compile to.
.global _main
.text

_main:
    mov     x0, 1
    adrp    x1, message@PAGE
    add     x1, x1, message@PAGEOFF
    mov     x2, 13
    bl      _write

    mov     x0, #0
    bl      _exit

.data
message:
    .ascii  "Hello there!\n"
