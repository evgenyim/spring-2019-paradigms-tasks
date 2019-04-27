#!/usr/bin/env python3
from model import *


def pretty_print(program):
    print(PrettyPrint().get_command(program))


class PrettyPrint(ASTNodeVisitor):
    INDENT = 4

    def __init__(self):
        self.indentation_level = 0

    def indent(self):
        return (PrettyPrint.INDENT * self.indentation_level) * ' '

    def get_command(self, program):
        ret = program.accept(self)
        if not ret.endswith('}'):
            ret += ';'
        return ret

    def add_definition(self, func):
        res = 'def ' + func.name + '('
        res += ', '.join(func.function.args)
        res += ') {\n'
        return res

    def visit_number(self, number):
        return str(number.value)

    def visit_function(self, function):
        raise TypeError("PrettyPrint shouldn't visit function")

    def visit_function_definition(self, func_def):
        res = self.add_definition(func_def)
        self.indentation_level += 1
        for expr in func_def.function.body:
            res += self.indent() + self.get_command(expr)
            res += '\n'
        self.indentation_level -= 1
        res += self.indent() + '}'
        return res

    def visit_conditional(self, conditional):
        res = 'if (' + conditional.condition.accept(self) + ') {\n'
        self.indentation_level += 1
        for expr in conditional.if_true or []:
            res += self.indent() + self.get_command(expr)
            res += '\n'
        self.indentation_level -= 1
        if not conditional.if_false:
            res += self.indent() + '}'
            return res
        res += self.indent() + '} else {\n'
        self.indentation_level += 1
        for expr in conditional.if_false or []:
            res += self.indent() + self.get_command(expr)
            res += '\n'
        self.indentation_level -= 1
        res += self.indent() + '}'
        return res

    def visit_print(self, print):
        return 'print ' + print.expr.accept(self)

    def visit_read(self, read):
        return 'read ' + read.name

    def visit_function_call(self, func_call):
        accepted_args = [expr.accept(self) for expr in func_call.args]
        res = func_call.fun_expr.accept(self) + '('
        res += ', '.join(accepted_args)
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
