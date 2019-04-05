#!/usr/bin/env python3
import pytest
from folder import *
from printer import *


def test_num_num():
    const_folder = ConstantFolder()
    check = const_folder.fold(BinaryOperation(Number(19), '-', Number(7)))
    assert check == Number(12)
    check = const_folder.fold(BinaryOperation(Number(11), '*', Number(9)))
    assert check == Number(99)


def test_num_ref():
    const_folder = ConstantFolder()
    check = const_folder.fold(BinaryOperation(Number(0),
                                              '*', Reference('tmp')))
    assert check == Number(0)


def test_ref_num():
    const_folder = ConstantFolder()
    check = const_folder.fold(BinaryOperation(Reference('tmp'),
                                              '*', Number(0)))
    assert check == Number(0)


def test_ref_ref():
    const_folder = ConstantFolder()
    check = const_folder.fold(BinaryOperation(Reference('tmp'),
                                              '-', Reference('tmp')))
    assert check == Number(0)


def test_un_num():
    const_folder = ConstantFolder()
    check = const_folder.fold(UnaryOperation('-', Number(10)))
    assert check == Number(-10)
    check = const_folder.fold(UnaryOperation('!', Number(10)))
    assert check == Number(0)


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
