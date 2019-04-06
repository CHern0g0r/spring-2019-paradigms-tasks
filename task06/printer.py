from model import *


def pretty_print(program):
    printer = PrettyPrint()
    print(printer.get_str(program))


class PrettyPrint(ASTNodeVisitor):

    def __init__(self):
        self.indent_size = 4
        self.indent_amount = 0

    def get_str(self, program):
        return program.accept(self) + ';'

    def get_indent(self):
        return ' ' * (self.indent_amount * self.indent_size)

    def visit_number(self, number):
        return str(number.value)

    def visit_function(self, function):
        pass

    def visit_print(self, print):
        return 'print ' + print.expr.accept(self)

    def visit_read(self, read):
        return 'read ' + read.name

    def visit_reference(self, reference):
        return reference.name

    def visit_un_operation(self, operation):
        return operation.op + '(' + operation.expr.accept(self) + ')'

    def visit_bin_operation(self, operation):
        l_val = operation.lhs.accept(self)
        r_val = operation.rhs.accept(self)
        return '(' + l_val + ') ' + operation.op + ' (' + r_val + ')'

    def visit_conditional(self, cond):
        res = 'if (' + cond.condition.accept(self) + ') {\n'
        self.indent_amount += 1
        for expr in cond.if_true or []:
            res += self.get_indent() + expr.accept(self) + ';\n'
        self.indent_amount -= 1
        if not cond.if_false:
            res += self.get_indent() + '}'
            return res
        res += self.get_indent() + '} else {\n'
        self.indent_amount += 1
        for expr in cond.if_false or []:
            res += self.get_indent() + expr.accept(self) + ';\n'
        self.indent_amount -= 1
        res += self.get_indent() + '}'
        return res

    def visit_function_call(self, func_call):
        res = func_call.fun_expr.accept(self) + '('
        if func_call.args:
            res += func_call.args.pop(0).accept(self)
            for expr in func_call.args:
                res += ', ' + expr.accept(self)
        res += ')'
        return res

    def visit_function_definition(self, func_def):
        res = 'def ' + func_def.name + '('
        func = func_def.function
        if func.args:
            res += func.args.pop(0)
            for arg in func.args:
                res += ', ' + arg
        res += ') {\n'
        self.indent_amount += 1
        for expr in func.body:
            res += self.get_indent() + expr.accept(self) + ';\n'
        self.indent_amount -= 1
        res += self.get_indent() + '}'
        return res
