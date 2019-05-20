from model import ASTNodeVisitor


def pretty_print(program):
    printer = PrettyPrinter()
    program.accept(printer)
    print(printer.get_data())


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.data = ""
        self.indent = 0

    def get_data(self):
        if not self.data.endswith('}'):
            self.data += ';'
        return self.data

    def make_indent(self):
        self.data += "\n" + "\t" * self.indent

    def visit_reference(self, node):
        self.data += node.name

    def visit_number(self, node):
        self.data += str(node.value)

    def visit_print(self, node):
        self.data += "print "
        node.expr.accept(self)

    def visit_read(self, node):
        self.data += "read {}".format(node.name)

    def visit_function(self, node):
        self.data += "{"
        self.visit_block(node.body)
        self.data += "}"

    def visit_function_definition(self, node):
        self.data += "def {}(".format(node.name)
        self.data += ", ".join(node.function.args)
        self.data += ") "
        node.function.accept(self)

    def visit_conditional(self, node):
        self.data += "if ("
        node.condition.accept(self)
        self.data += ") {"
        self.visit_block(node.if_true)
        self.data += "}"
        if node.if_false:
            self.data += " else {"
            self.visit_block(node.if_false)
            self.data += "}"

    def visit_function_call(self, node):
        node.fun_expr.accept(self)
        self.data += "("
        for i, argument in enumerate(node.args):
            if i != 0:
                self.data += ", "
            argument.accept(self)
        self.data += ")"

    def visit_binary_operation(self, node):
        self.data += "("
        node.lhs.accept(self)
        self.data += ")"
        self.data += " {} ".format(node.op)
        self.data += "("
        node.rhs.accept(self)
        self.data += ")"

    def visit_unary_operation(self, node):
        self.data += "{}".format(node.op)
        self.data += "("
        node.expr.accept(self)
        self.data += ")"

    def visit_block(self, block):
        if not block:
            self.make_indent()
            return
        self.indent += 1
        self.make_indent()
        for i, node in enumerate(block):
            if i != 0:
                self.make_indent()
            node.accept(self)
            if not self.data.endswith('}'):
                self.data += ";"
        self.indent -= 1
        self.make_indent()
