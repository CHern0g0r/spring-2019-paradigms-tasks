#!/usr/bin/env python3
import pytest
from printer import *


def test_conditional():
    printer = PrettyPrint()
    check = printer.get_str(Conditional(Number(42), [], []))
    assert check == 'if (42) {\n};'


def test_function_definition():
    printer = PrettyPrint()
    check = printer.get_str(FunctionDefinition("bar", Function([], [])))
    assert check == 'def bar() {\n};'


def test_print():
    printer = PrettyPrint()
    check = printer.get_str(Print(Number(42)))
    assert check == 'print 42;'


def test_read():
    printer = PrettyPrint()
    check = printer.get_str(Read('data'))
    assert check == 'read data;'


def test_number():
    printer = PrettyPrint()
    check = printer.get_str(Number(42))
    assert check == '42;'


def test_reference():
    printer = PrettyPrint()
    check = printer.get_str(Reference('data'))
    assert check == 'data;'


def test_bin_operation():
    printer = PrettyPrint()
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(7), '*', add)
    check = printer.get_str(mul)
    assert check == '(7) * ((2) + (3));'


def test_un_operation():
    printer = PrettyPrint()
    check = printer.get_str(UnaryOperation('!', Number(42)))
    assert check == '!(42);'


def test_function_call():
    printer = PrettyPrint()
    check = printer.get_str(FunctionCall(Reference('bar'),
                                        [Number(4), Number(2)]))
    assert check == 'bar(4, 2);'


def test_all(capsys):
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))
    assert capsys.readouterr().out == 'def main(arg1) {\n' + \
                                      '    read x;\n' + \
                                      '    print x;\n' + \
                                      '    if ((2) == (3)) {\n' + \
                                      '        if (1) {\n' + \
                                      '        };\n' + \
                                      '    } else {\n' + \
                                      '        exit(-(arg1));\n' + \
                                      '    };\n' + \
                                      '};\n'


if __name__ == "__main__":
    pytest.main()