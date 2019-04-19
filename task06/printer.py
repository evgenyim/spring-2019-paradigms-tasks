#!/usr/bin/env python3
from model import *


def pretty_print(program):
    printer = PrettyPrint()
    print(printer.get_result(program))


class PrettyPrint(ASTNodeVisitor):

    def __init__(self):
        self.indentation_level = 0
        self.INDENT = 4

    def indent(self):
        if self.indentation_level:
            return (self.INDENT * self.indentation_level) * ' '
        else:
            return ''

    def get_result(self, program):
        ret = program.accept(self)
        if not ret.endswith('}'):
            ret += ';'
        return ret

    def visit_number(self, number):
        return str(number.value)

    def visit_function(self, function):
        return TypeError

    def visit_function_definition(self, func_def):
        res = 'def ' + func_def.name + '('
        res += ', '.join(func_def.function.args)
        res += ') {\n'
        self.indentation_level += 1
        for expr in func_def.function.body:
            res += self.indent() + expr.accept(self)
            if not res.endswith('}'):
                res += ';'
            res += '\n'
        self.indentation_level -= 1
        res += self.indent() + '}'
        return res

    def visit_conditional(self, conditional):
        res = 'if (' + conditional.condition.accept(self) + ') {\n'
        self.indentation_level += 1
        for expr in conditional.if_true or []:
            res += self.indent() + expr.accept(self)
            if not res.endswith('}'):
                res += ';'
            res += '\n'
        self.indentation_level -= 1
        if not conditional.if_false:
            res += self.indent() + '}'
            return res
        res += self.indent() + '} else {\n'
        self.indentation_level += 1
        for expr in conditional.if_false or []:
            res += self.indent() + expr.accept(self)
            if not res.endswith('}'):
                res += ';'
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
