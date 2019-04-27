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
        return Function(
            function.args.copy(),
            [expr.accept(self) for expr in function.body or []]
        )

    def visit_function_definition(self, func_def):
        return FunctionDefinition(
            func_def.name,
            func_def.function.accept(self)
        )

    def visit_conditional(self, conditional):
        return Conditional(
            conditional.condition.accept(self),
            [expr.accept(self) for expr in conditional.if_true or []],
            [expr.accept(self) for expr in conditional.if_false or []]
        )

    def visit_print(self, print):
        return Print(print.expr.accept(self))

    def visit_read(self, read):
        return Read(read.name)

    def visit_function_call(self, func_call):
        return FunctionCall(
            func_call.fun_expr.accept(self),
            [expr.accept(self) for expr in func_call.args or []]
        )

    def visit_reference(self, reference):
        return Reference(reference.name)

    def visit_binary_operation(self, bin_op):
        lhs = bin_op.lhs.accept(self)
        rhs = bin_op.rhs.accept(self)
        op = bin_op.op
        if isinstance(lhs, Number) and isinstance(rhs, Number):
            return BinaryOperation(lhs, op, rhs).evaluate(Scope())
        if (isinstance(lhs, Number) and lhs == Number(0) and op == '*' and
                isinstance(rhs, Reference)) or \
            (isinstance(rhs, Number) and rhs == Number(0) and op == '*' and
                isinstance(lhs, Reference)):
            return Number(0)
        if isinstance(lhs, Reference) and \
           isinstance(rhs, Reference) and op == '-':
            if lhs.name == rhs.name:
                return Number(0)
        return BinaryOperation(lhs, op, rhs)

    def visit_unary_operation(self, un_op):
        expr = un_op.expr.accept(self)
        op = un_op.op
        ret = UnaryOperation(op, expr)
        if isinstance(expr, Number):
            ret = ret.evaluate(Scope())
        return ret
