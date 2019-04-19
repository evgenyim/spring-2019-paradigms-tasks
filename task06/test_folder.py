#!/usr/bin/env python3
import pytest
from folder import *
from printer import pretty_print


def test_num_num():
    const_folder = ConstantFolder()
    actual = const_folder.visit(BinaryOperation(Number(19), '-', Number(7)))
    assert actual == Number(12)
    actual = const_folder.visit(BinaryOperation(Number(11), '*', Number(9)))
    assert actual == Number(99)


def test_num_ref():
    const_folder = ConstantFolder()
    actual = const_folder.visit(BinaryOperation(Number(0),
                                                '*', Reference('tmp')))
    assert actual == Number(0)


def test_ref_num():
    const_folder = ConstantFolder()
    actual = const_folder.visit(BinaryOperation(Reference('tmp'),
                                                '*', Number(0)))
    assert actual == Number(0)


def test_ref_ref():
    const_folder = ConstantFolder()
    actual = const_folder.visit(BinaryOperation(Reference('tmp'),
                                                '-', Reference('tmp')))
    assert actual == Number(0)


def test_un_num():
    const_folder = ConstantFolder()
    actual = const_folder.visit(UnaryOperation('-', Number(10)))
    assert actual == Number(-10)
    actual = const_folder.visit(UnaryOperation('!', Number(10)))
    assert actual == Number(0)


def test_all(capsys):
    pretty_print(fold_constants(
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
    ))
    assert capsys.readouterr().out == '13;\n'


if __name__ == "__main__":
    pytest.main()
