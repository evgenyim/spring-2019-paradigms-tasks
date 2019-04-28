#!/usr/bin/env python3
import pytest
from folder import *
from printer import pretty_print


def test_num_num():
    actual = ConstantFolder().visit(
        BinaryOperation(Number(19), '-', Number(7))
    )
    assert actual == Number(12)
    actual = ConstantFolder().visit(
        BinaryOperation(Number(11), '*', Number(9))
    )
    assert actual == Number(99)


def test_zero_mul_ref():
    actual = ConstantFolder().visit(
        BinaryOperation(Number(0), '*', Reference('tmp'))
    )
    assert actual == Number(0)


def test_ref_mul_zero():
    actual = ConstantFolder().visit(
        BinaryOperation(Reference('tmp'), '*', Number(0))
    )
    assert actual == Number(0)


def test_ref_minus_ref():
    actual = ConstantFolder().visit(
        BinaryOperation(Reference('tmp'), '-', Reference('tmp'))
    )
    assert actual == Number(0)


def test_un_num():
    actual = ConstantFolder().visit(UnaryOperation('-', Number(10)))
    assert actual == Number(-10)
    actual = ConstantFolder().visit(UnaryOperation('!', Number(10)))
    assert actual == Number(0)


def test_all():
    actual = fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    )
    assert actual == Number(13)


if __name__ == "__main__":
    pytest.main()
