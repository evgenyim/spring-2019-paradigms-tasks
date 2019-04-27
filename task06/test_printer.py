#!/usr/bin/env python3
import pytest
from printer import *
import textwrap


def test_conditional():
    check = PrettyPrint().visit_conditional(Conditional(Number(42), [], []))
    assert check == 'if (42) {\n}'


def test_function_definition():
    check = PrettyPrint().visit_function_definition(FunctionDefinition("foo", Function([], [])))
    assert check == 'def foo() {\n}'


def test_print():
    check = PrettyPrint().visit_print(Print(Number(42)))
    assert check == 'print 42'


def test_read():
    check = PrettyPrint().visit_read(Read('x'))
    assert check == 'read x'


def test_number():
    check = PrettyPrint().visit_number(Number(10))
    assert check == '10'


def test_reference():
    check = PrettyPrint().visit_reference(Reference('x'))
    assert check == 'x'


def test_bin_operation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    check = PrettyPrint().visit_binary_operation(mul)
    assert check == '(1) * ((2) + (3))'


def test_un_operation():
    check = PrettyPrint().visit_unary_operation(UnaryOperation('-', Number(42)))
    assert check == '-(42)'


def test_function_call():
    check = PrettyPrint().visit_function_call(FunctionCall(Reference('foo'),
                                        [Number(1), Number(2), Number(3)]))
    assert check == 'foo(1, 2, 3)'


def test_all(capsys):
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))
    expected = '''\
        def main(arg1) {
            read x;
            print x;
            if ((2) == (3)) {
                if (1) {
                }
            } else {
                exit(-(arg1));
            }
        }
    '''

    assert capsys.readouterr().out == textwrap.dedent(expected)


if __name__ == '__main__':
    pytest.main()
