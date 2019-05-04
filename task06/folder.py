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
            [stmt.accept(self) for stmt in function.body]
        )

    def visit_function_definition(self, func_def):
        return FunctionDefinition(
            func_def.name,
            func_def.function.accept(self)
        )

    def visit_conditional(self, conditional):
        return Conditional(
            conditional.condition.accept(self),
            [stmt.accept(self) for stmt in conditional.if_true or []],
            [stmt.accept(self) for stmt in conditional.if_false or []]
        )

    def visit_print(self, print):
        return Print(print.expr.accept(self))

    def visit_read(self, read):
        return Read(read.name)

    def visit_function_call(self, func_call):
        return FunctionCall(
            func_call.fun_expr.accept(self),
            [stmt.accept(self) for stmt in func_call.args or []]
        )

    def visit_reference(self, reference):
        return Reference(reference.name)

    def visit_binary_operation(self, bin_op):
        lhs = bin_op.lhs.accept(self)
        rhs = bin_op.rhs.accept(self)
        op = bin_op.op
        if isinstance(lhs, Number) and isinstance(rhs, Number):
            return BinaryOperation(lhs, op, rhs).evaluate(Scope())
        if ((op == '*' and isinstance(lhs, Number) and lhs == Number(0) and
                isinstance(rhs, Reference)) or
            (op == '*' and isinstance(rhs, Number) and rhs == Number(0) and
                isinstance(lhs, Reference))):
            return Number(0)
        if (op == '-' and
            isinstance(lhs, Reference) and isinstance(rhs, Reference) and
                lhs.name == rhs.name):
            return Number(0)
        return BinaryOperation(lhs, op, rhs)

    def visit_unary_operation(self, un_op):
        expr = un_op.expr.accept(self)
        op = un_op.op
        if isinstance(expr, Number):
            return UnaryOperation(op, expr).evaluate(Scope())
        return UnaryOperation(op, expr)
