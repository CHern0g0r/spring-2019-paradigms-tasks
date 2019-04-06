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

    def visit_reference(self, ref):
        return Reference(ref.name)

    def visit_function(self, func):
        body = []
        for expr in func.body:
            body.append(expr.accept(self))
        return Function(func.args, body)

    def visit_print(self, pr):
        return Print(pr.expr.accept(self))

    def visit_read(self, read):
        return Read(read.name)

    def visit_function_definition(self, definition):
        func = definition.func.accept(self)
        return FunctionDefinition(definition.name, func)

    def visit_function_call(self, func_call):
        func = func_call.fun_expr.accept(self)
        args = []
        for expr in func_call.args:
            args.append(expr.accept(self))
        return FunctionCall(func, args)

    def visit_un_operation(self, operation):
        expr = operation.expr.accept(self)
        return UnaryOperation(operation.op, expr).evaluate(None)

    def visit_bin_operation(self, operation):
        l_val = operation.lhs.accept(self)
        r_val = operation.rhs.accept(self)
        if isinstance(l_val, Number) and l_val == Number(0) and\
           operation.op == '*' or\
           isinstance(r_val, Number) and r_val == Number(0) and\
           operation.op == '*':
            return Number(0)
        if isinstance(l_val, Reference) and isinstance(r_val, Reference) and\
           operation.op == '-' and l_val.name == r_val.name:
            return Number(0)
        if isinstance(l_val, Number) and isinstance(r_val, Number):
            return BinaryOperation(l_val, operation.op, r_val).evaluate(None)
        return BinaryOperation(l_val, operation.op, r_val)

    def visit_conditional(self, conditional):
        condition = conditional.condition.accept(self)
        if_true = []
        if_false = []
        for expr in conditional.if_true:
            if_true.append(expr.accept(self))
        for expr in conditional.if_false:
            if_false.append(expr.accept(self))
        return Conditional(condition, if_true, if_false)
