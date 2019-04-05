#!/usr/bin/env   python3
from model import *


def fold_constants(program):
    const_folder = ConstantFolder()
    return const_folder.fold(program)


class ConstantFolder(ASTNodeVisitor):
    def fold(self, program):
        return program.accept(self)

    def visit_number(self, number):
        return Number(number.value)

    def visit_function(self, function):
        body = []
        for expr in function.body:
            body.append(expr.accept(self))
        return Function(function.args, body)

    def visit_function_definition(self, func_def):
        function = func_def.function.accept(self)
        return FunctionDefinition(func_def.name, function)

    def visit_conditional(self, conditional):
        condition = conditional.condition.accept(self)
        if_true = []
        if_false = []
        for expr in conditional.if_true:
            if_true.append(expr.accept(self))
        for expr in conditional.if_false:
            if_false.append(expr.accept(self))
        return Conditional(condition, if_true, if_false)

    def visit_print(self, print):
        return Print(print.expr.accept(self))

    def visit_read(self, read):
        return Read(read.name)

    def visit_function_call(self, func_call):
        function = func_call.fun_expr.accept(self)
        args = []
        for expr in func_call.args:
            args.append(expr.accept(self))
        return FunctionCall(function, args)

    def visit_reference(self, reference):
        return Reference(reference.name)

    def visit_bin_operation(self, bin_op):
        lhs = bin_op.lhs.accept(self)
        rhs = bin_op.rhs.accept(self)
        op = bin_op.op
        if isinstance(lhs, Number) and isinstance(rhs, Number):
            return BinaryOperation(lhs, op, rhs).evaluate(None)
        # if isinstance(lhs, Number)
        if isinstance(lhs, Number) and lhs == Number(0) and op == '*' or \
           isinstance(rhs, Number) and rhs == Number(0) and op == '*':
            return Number(0)
        if isinstance(lhs, Reference) and \
           isinstance(rhs, Reference) and op == '-':
            if lhs.name == rhs.name:
                return Number(0)
        return BinaryOperation(lhs, op, rhs)

    def visit_un_operation(self, un_op):
        expr = un_op.expr.accept(self)
        op = un_op.op
        return UnaryOperation(op, expr).evaluate(None)
