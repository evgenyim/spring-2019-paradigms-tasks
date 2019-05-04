#!/usr/bin/env python3
import pytest
from printer import *
import textwrap


def test_conditional():
    formatted = Conditional(Number(42), None, []).accept(PrettyPrint())
    assert formatted == 'if (42) {\n}'


def test_conditional_2():
    formatted = Conditional(Number(42), [Print(Number(3))], [Print(Number(4))]).accept(PrettyPrint())
    assert formatted == textwrap.dedent('''\
        if (42) {
            print 3;
        } else {
            print 4;
        }''')


def test_function_definition():
    formatted = PrettyPrint().visit_function_definition(
        FunctionDefinition('foo', Function([], []))
    )
    assert formatted == 'def foo() {\n}'


def test_print():
    formatted = PrettyPrint().visit_print(Print(Number(42)))
    assert formatted == 'print 42'


def test_read():
    formatted = PrettyPrint().visit_read(Read('x'))
    assert formatted == 'read x'


def test_number():
    formatted = PrettyPrint().visit_number(Number(10))
    assert formatted == '10'


def test_reference():
    formatted = PrettyPrint().visit_reference(Reference('x'))
    assert formatted == 'x'


def test_bin_operation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    formatted = PrettyPrint().visit_binary_operation(mul)
    assert formatted == '(1) * ((2) + (3))'


def test_un_operation():
    formatted = PrettyPrint().visit_unary_operation(
        UnaryOperation('-', Number(42))
    )
    assert formatted == '-(42)'


def test_function_call():
    formatted = PrettyPrint().visit_function_call(FunctionCall(
        Reference('foo'), [Number(1), Number(2), Number(3)]))
    assert formatted == 'foo(1, 2, 3)'


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
    expected = textwrap.dedent('''\
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
    ''')

    assert capsys.readouterr().out == expected


if __name__ == '__main__':
    pytest.main()
