from . import ir
from .conftest import IRTester, compile_and_run_success, strip
from .ir_remove_duplicate_getptr import ir_remove_duplicate_getptr


def test_happy_path(ir_tester: IRTester) -> None:
    t = ir_tester
    src = t.reg(t.int())
    get_ptr1 = t.reg(t.int())
    get_ptr2 = t.reg(t.int())
    get_ptr3 = t.reg(t.int())
    load1 = t.reg(t.int())
    load2 = t.reg(t.int())
    block2 = t.block(
        [
            ir.GetPtr(reg=get_ptr1, src=src, field=1),
            ir.Load(reg=load1, src=get_ptr1),
            ir.GetPtr(reg=get_ptr2, src=src, field=1),
            ir.Load(reg=load2, src=get_ptr2),
            # An unrelated GetPtr as it points to a different field of the same struct.
            ir.GetPtr(reg=get_ptr3, src=src, field=2),
        ],
        None,
    )
    block1 = t.block(
        [ir.Alloc(reg=src, args=[])],
        ir.Jump(block2),
    )
    fn_ir = t.fn_ir([block1, block2])

    ir_remove_duplicate_getptr(fn_ir)

    assert block1.insts == [
        ir.Alloc(reg=src, args=[]),
        ir.GetPtr(reg=get_ptr1, src=src, field=1),
    ]
    assert block2.insts == [
        ir.Load(reg=load1, src=get_ptr1),
        ir.Load(reg=load2, src=get_ptr1),
        ir.GetPtr(reg=get_ptr3, src=src, field=2),
    ]


def test_non_duplicate_getptr_is_not_hoisted(ir_tester: IRTester) -> None:
    t = ir_tester
    src1 = t.reg(t.int())
    get_ptr1 = t.reg(t.int())
    load1 = t.reg(t.int())
    block2 = t.block(
        [
            ir.GetPtr(reg=get_ptr1, src=src1, field=1),
            ir.Load(reg=load1, src=get_ptr1),
        ],
        None,
    )
    block1 = t.block(
        [ir.Alloc(reg=src1, args=[])],
        ir.Jump(block2),
    )
    fn_ir = t.fn_ir([block1, block2])

    ir_remove_duplicate_getptr(fn_ir)

    assert block1.insts == [
        ir.Alloc(reg=src1, args=[]),
    ]
    assert block2.insts == [
        ir.GetPtr(reg=get_ptr1, src=src1, field=1),
        ir.Load(reg=load1, src=get_ptr1),
    ]


def test_compile_with_branches() -> None:
    stdout = compile_and_run_success(
        """
        struct Value {
            value Str
        }

        fn main() {
            let v = Value("PASS")
            -- The first GetPtr will be created in the following branch that is not taken.
            if false => print(v.value)
            -- A naive implementation would crash here.
            print(v.value)
        }
        """
    )
    assert stdout == strip(
        """
        PASS
        """
    )
