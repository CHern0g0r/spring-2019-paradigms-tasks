#!/usr/bin/env python3
import pytest
from model import *
from printer import *


def test_print(capsys):
    func_print = Print(Number(100))
    pretty_print(func_print)
    captured = capsys.readouterr()
    assert captured.out == "print 100;\n"


def test_read(capsys):
    func_read = Read('x')
    pretty_print(func_read)
    captured = capsys.readouterr()
    assert captured.out == "read x;\n"


def test_number(capsys):
    number = Number(10)
    pretty_print(number)
    captured = capsys.readouterr()
    assert captured.out == "10;\n"


def test_reference(capsys):
    refer = Reference('x')
    pretty_print(refer)
    captured = capsys.readouterr()
    assert captured.out == "x;\n"


def test_function_definition(capsys):
    func = FunctionDefinition("foo", Function([], []))
    pretty_print(func)
    captured = capsys.readouterr()
    assert captured.out == "def foo() {\n}\n"


def test_conditional(capsys):
    condition = Conditional(Number(100), [], [])
    pretty_print(condition)
    captured = capsys.readouterr()
    assert captured.out == "if (100) {\n}\n"


def test_functioncall(capsys):
    scope = Scope()
    FunctionDefinition(
        'func', Function(['n'], [Reference('n')])).evaluate(scope)
    call = FunctionCall(Reference('func'), [
        Number(10)
    ])
    pretty_print(call)
    captured = capsys.readouterr()
    assert captured.out == "func(10);\n"


def test_binary_operation(capsys):
    operation = BinaryOperation(Number(1), "*",
                                BinaryOperation(Number(11), "+", Number(111)))
    pretty_print(operation)
    captured = capsys.readouterr()
    assert captured.out == "(1) * ((11) + (111));\n"


def test_unary_operation(capsys):
    operation = UnaryOperation('-', Number(100))
    pretty_print(operation)
    captured = capsys.readouterr()
    assert captured.out == "-(100);\n"


def test_end_to_end_1(capsys):
    program = FunctionDefinition('main', Function(['arg1'], [
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
    ]))

    pretty_print(program)
    captured = capsys.readouterr()
    assert captured.out == "def main(arg1) {\n\tread x;\n\tprint x;\n\tif \
((2) == (3)) {\n\t\tif (1) {\n\t\t}\n\t} else {\n\t\texit(-(arg1));\
\n\t}\n}\n"


def test_end_to_end_2(capsys):
    program = FunctionDefinition(
        'factorial', Function(['n'], [
            Conditional(
                BinaryOperation(Reference('n'),
                                '==',
                                Number(0)),
                [Number(1)],
                [
                    BinaryOperation(
                        Reference('n'),
                        '*',
                        FunctionCall(
                            Reference('factorial'), [
                                BinaryOperation(
                                    Reference('n'),
                                    '-',
                                    Number(1))
                            ])
                    )
                ])
        ]))

    pretty_print(program)
    captured = capsys.readouterr()
    assert captured.out == "def factorial(n) {\n\tif ((n) == (0)) \
{\n\t\t1;\n\t} else {\n\t\t(n) * (factorial((n) - (1)));\n\t}\n}\n"
