#!/usr/bin/env python3
from model import *


def fold_constants(program):
    return ConstantFolder().visit(program)


class ConstantFolder(ASTNodeVisitor):
    def visit(self, program):
        return program.accept(self)

    def visit_number(self, number):
        return Number(number.value)

    def visit_function(self, function):
        body = [expr.accept(self) for expr in function.body or []]
        return Function(function.args, body)

    def visit_function_definition(self, func_def):
        function = func_def.function.accept(self)
        return FunctionDefinition(func_def.name, function)

    def visit_conditional(self, conditional):
        condition = conditional.condition.accept(self)
        if_true = [expr.accept(self) for expr in conditional.if_true or []]
        if_false = [expr.accept(self) for expr in conditional.if_false or []]
        return Conditional(condition, if_true, if_false)

    def visit_print(self, print):
        return Print(print.expr.accept(self))

    def visit_read(self, read):
        return Read(read.name)

    def visit_function_call(self, func_call):
        function = func_call.fun_expr.accept(self)
        args = [expr.accept(self) for expr in func_call.args]
        return FunctionCall(function, args)

    def visit_reference(self, reference):
        return Reference(reference.name)

    def visit_binary_operation(self, bin_op):
        lhs = bin_op.lhs.accept(self)
        rhs = bin_op.rhs.accept(self)
        op = bin_op.op
        scope = Scope()
        if isinstance(lhs, Number) and isinstance(rhs, Number):
            return BinaryOperation(lhs, op, rhs).evaluate(scope)
        if isinstance(lhs, Number) and lhs == Number(0) and op == '*' or \
           isinstance(rhs, Number) and rhs == Number(0) and op == '*':
            return Number(0)
        if isinstance(lhs, Reference) and \
           isinstance(rhs, Reference) and op == '-':
            if lhs.name == rhs.name:
                return Number(0)
        return BinaryOperation(lhs, op, rhs)

    def visit_unary_operation(self, un_op):
        expr = un_op.expr.accept(self)
        op = un_op.op
        scope = Scope()
        ret = UnaryOperation(op, expr)
        if isinstance(expr, Number):
            ret = ret.evaluate(scope)
        return ret
