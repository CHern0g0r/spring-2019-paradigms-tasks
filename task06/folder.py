from model import *


def fold_constants(program):
    return program.accept(ConstantFolder())


class ConstantFolder(ASTNodeVisitor):
    def visit_number(self, node):
        return Number(node.value)

    def visit_reference(self, node):
        return Reference(node.name)

    def visit_print(self, node):
        return Print(node.expr.accept(self))

    def visit_read(self, node):
        return Read(node.name)

    def visit_function(self, node):
        body = [statement.accept(self) for statement in node.body]
        return Function(node.args, body)

    def visit_function_definition(self, node):
        return FunctionDefinition(node.name, node.function.accept(self))

    def visit_function_call(self, node):
        fun_expr = node.fun_expr.accept(self)
        args = [argument.accept(self) for argument in node.args]
        return FunctionCall(fun_expr, args)

    def visit_conditional(self, node):
        condition = node.condition.accept(self)
        if_true = [statement.accept(self) for statement
                   in node.if_true or []]
        if_false = [statement.accept(self) for statement
                    in node.if_false or []]
        return Conditional(condition, if_true, if_false)

    def visit_binary_operation(self, node):
        lhs = node.lhs.accept(self)
        rhs = node.rhs.accept(self)
        op = node.op
        if isinstance(lhs, Number) and isinstance(rhs, Number):
            return BinaryOperation(lhs, op, rhs).evaluate(Scope())
        if (op == '*' and isinstance(lhs, Number) and lhs == Number(0) and
                isinstance(rhs, Reference)):
            return Number(0)
        if (op == '*' and isinstance(rhs, Number) and rhs == Number(0) and
                isinstance(lhs, Reference)):
            return Number(0)
        if (op == '-' and isinstance(lhs, Reference) and
                isinstance(rhs, Reference) and lhs.name == rhs.name):
            return Number(0)
        return BinaryOperation(lhs, op, rhs)

    def visit_unary_operation(self, node):
        op = node.op
        expr = node.expr.accept(self)
        if isinstance(expr, Number):
            return UnaryOperation(op, expr).evaluate(Scope())
        return UnaryOperation(op, expr)
