#!/usr/bin/env   python3
from model import *

INDENT = 4


def pretty_print(program):
    printer = PrettyPrint()
    print(printer.result(program))


class PrettyPrint(ASTVisitor):

    def __init__(self):
        self.indent_amount = 0

    def indent(self):
        if self.indent_amount:
            return (INDENT * self.indent_amount) * ' '
        else:
            return ''

    def result(self, program):
        return program.accept(self) + ';'

    def visit_number(self, number):
        return str(number.value)

    def visit_function(self, function):
        return 1

    def visit_function_definition(self, func_def):
        res = self.indent() + 'def ' + func_def.name + '('
        if not func_def.function.args:
            res += ') {\n'
        else:
            res += func_def.function.args.pop(0)
            for arg in func_def.function.args:
                res += ', ' + arg
            res += ') {\n'
        self.indent_amount += 1
        for expr in func_def.function.body:
            res += self.indent() + expr.accept(self) + ';\n'
        self.indent_amount -= 1
        res += self.indent() + '}'
        return res

    def visit_conditional(self, conditional):
        res = 'if (' + conditional.condition.accept(self) + ') {\n'
        self.indent_amount += 1
        for expr in conditional.if_true or []:
            res += self.indent() + expr.accept(self) + ';\n'
        self.indent_amount -= 1
        if not conditional.if_false:
            res += self.indent() + '}'
            return res
        res += self.indent() + '} else {\n'
        self.indent_amount += 1
        for expr in conditional.if_false or []:
            res += self.indent() + expr.accept(self) + ';\n'
        self.indent_amount -= 1
        res += self.indent() + '}'
        return res

    def visit_print(self, print):
        return 'print ' + print.expr.accept(self)

    def visit_read(self, read):
        return 'read ' + read.name

    def visit_function_call(self, func_call):
        res = func_call.fun_expr.accept(self) + '('
        if func_call.args:
            res += func_call.args.pop(0).accept(self)
            for arg in func_call.args:
                res += ', ' + arg.accept(self)
        res += ')'
        return res

    def visit_reference(self, reference):
        return reference.name

    def visit_bin_operation(self, bin_op):
        lhs = bin_op.lhs.accept(self)
        rhs = bin_op.rhs.accept(self)
        return '(' + lhs + ')' + ' ' + bin_op.op + ' ' + '(' + rhs + ')'

    def visit_un_operation(self, un_op):
        return un_op.op + '(' + un_op.expr.accept(self) + ')'
