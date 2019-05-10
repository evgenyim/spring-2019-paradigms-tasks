#!/usr/bin/env python3
from model import *


def pretty_print(program):
    print(PrettyPrint().get_statement(program))


class PrettyPrint(ASTNodeVisitor):
    INDENT = 4

    def __init__(self):
        self.indentation_level = 0

    def indent(self):
        return (PrettyPrint.INDENT * self.indentation_level) * ' '

    def get_statement(self, program):
        res = program.accept(self)
        if not res.endswith('}'):
            res += ';'
        return res

    def visit_block(self, args):
        res = '{\n'
        self.indentation_level += 1
        for expr in args or []:
            res += self.indent() + self.get_statement(expr)
            res += '\n'
        self.indentation_level -= 1
        res += self.indent() + '}'
        return res

    def visit_number(self, number):
        return str(number.value)

    def visit_function(self, function):
        raise TypeError('PrettyPrint shouldn\'t visit Function')

    def visit_function_definition(self, func_def):
        res = 'def ' + func_def.name + '('
        res += ', '.join(func_def.function.args)
        res += ') '
        res += self.visit_block(func_def.function.body)
        return res

    def visit_conditional(self, conditional):
        res = 'if (' + conditional.condition.accept(self) + ') '
        res += self.visit_block(conditional.if_true)
        if not conditional.if_false:
            return res
        res += ' else '
        res += self.visit_block(conditional.if_false)
        return res

    def visit_print(self, print):
        return 'print ' + print.expr.accept(self)

    def visit_read(self, read):
        return 'read ' + read.name

    def visit_function_call(self, func_call):
        res = func_call.fun_expr.accept(self) + '('
        res += ', '.join(expr.accept(self) for expr in func_call.args)
        res += ')'
        return res

    def visit_reference(self, reference):
        return reference.name

    def visit_binary_operation(self, bin_op):
        lhs = bin_op.lhs.accept(self)
        rhs = bin_op.rhs.accept(self)
        return '(' + lhs + ')' + ' ' + bin_op.op + ' ' + '(' + rhs + ')'

    def visit_unary_operation(self, un_op):
        return un_op.op + '(' + un_op.expr.accept(self) + ')'
