#!/usr/bin/env python3
import pytest
from folder import *
from printer import pretty_print


def test_num_num():
    assert ConstantFolder().visit(
        BinaryOperation(Number(19), '-', Number(7))
    ) == Number(12)
    assert ConstantFolder().visit(
        BinaryOperation(Number(11), '*', Number(9))
    ) == Number(99)


def test_zero_mul_ref():
    assert ConstantFolder().visit(
        BinaryOperation(Number(0), '*', Reference('tmp'))
    ) == Number(0)


def test_ref_mul_zero():
    assert ConstantFolder().visit(
        BinaryOperation(Reference('tmp'), '*', Number(0))
    ) == Number(0)


def test_ref_minus_ref():
    assert ConstantFolder().visit(
        BinaryOperation(Reference('tmp'), '-', Reference('tmp'))
    ) == Number(0)


def test_un_num():
    assert ConstantFolder().visit(
        UnaryOperation('-', Number(10))
    ) == Number(-10)
    assert ConstantFolder().visit(
        UnaryOperation('!', Number(10))
    ) == Number(0)


def test_all():
    assert fold_constants(
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
    ) == Number(13)


if __name__ == '__main__':
    pytest.main()
