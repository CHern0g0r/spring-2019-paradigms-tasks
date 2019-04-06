#!/usr/bin/env python3
import pytest
from model import *


def test_in_scope():
    a, b = object(), object()
    scope = Scope()
    scope['foo'] = a
    scope['bar'] = b
    check = scope['foo']
    assert check == a
    check = scope['bar']
    assert check == b


def test_parent():
    a, b = object(), object()
    main_parent = Scope()
    main_parent['foo'] = a
    parent = Scope(main_parent)
    scope = Scope(parent)
    scope['bar'] = b
    check = scope['foo']
    assert check == a


def test_exception():
    a = object()
    scope = Scope()
    scope['foo'] = a
    with pytest.raises(KeyError):
        check = scope['bar']


def test_print(capsys):
    scope = Scope()
    f = Print(Number(4))
    f.evaluate(scope)
    captured = capsys.readouterr()
    assert captured.out == '4\n'


def test_read(monkeypatch):
    scope = Scope()
    f = Read('foo')
    monkeypatch.setattr('builtins.input', lambda: '16')
    ret = f.evaluate(scope)
    assert ret.value == 16
    assert scope['foo']


def test_function_definition():
    scope = Scope()
    func = FunctionDefinition("bar", Function(["a"],
                              [
                                Print(Reference("a")),
                              ]
    ))
    func.evaluate(scope)
    assert scope['bar']


def test_binary_operation():
    a = Number(10)
    b = Number(15)
    scope = Scope()
    assert BinaryOperation(a, '+', b).evaluate(scope).value == 25
    assert BinaryOperation(a, '*', b).evaluate(scope).value == 150
    assert BinaryOperation(a, '-', b).evaluate(scope).value == -5
    assert not BinaryOperation(a, '>', b).evaluate(scope).value


def test_unary_operation():
    a = Number(7)
    scope = Scope()
    assert UnaryOperation('-', a).evaluate(scope).value == -7
    assert not UnaryOperation('!', a).evaluate(scope).value


def test_function_call(capsys):
    operation1 = FunctionDefinition(
        "foo",
        Function(["a", "b"],
                 [
                  Print(BinaryOperation(Reference("a"), "+", Reference("b"))),
                 ]
                 )
    )
    operation2 = FunctionCall(Reference("foo"), [
        Number(1),
        BinaryOperation(Number(2), "+", Number(3))
    ])
    s = Scope()
    operation1.evaluate(s)
    operation2.evaluate(s)
    assert capsys.readouterr().out == '6\n'


def test_reference():
    scope = Scope()
    scope['a'] = 10
    assert Reference('a').evaluate(scope) == 10


def test_conditional():
    a = Conditional(BinaryOperation(Number(5), '>', Number(4)),
                    [Number(1), Number(2)])
    scope = Scope()
    check = a.evaluate(scope)
    assert check.value == 2
    b = Conditional(BinaryOperation(Number(5), '<', Number(4)),
                    [Number(1), Number(2)])
    check = b.evaluate(scope)
    assert not check


def test_frac():
    func = FunctionDefinition(
        'fact',
        Function(['a'],
                 [Conditional(BinaryOperation(Reference('a'), '>', Number(1)),
                  [BinaryOperation(
                      FunctionCall(
                          Reference('fact'),
                          [BinaryOperation(Reference('a'), '-', Number(1))]),
                      '*',
                      Reference('a'))
                   ],
                  [Number(1)]
                  )
                  ]
                 )
    )
    scope = Scope()
    func_call = FunctionCall(Reference('fact'), [Number(5)])
    func.evaluate(scope)
    ret = func_call.evaluate(scope)
    assert ret.value == 120


def test_construction():
    FunctionDefinition('fac', Function(['n'], [
        Conditional(
            BinaryOperation(Reference('n'), '==', Number(0)),
            [Number(1)],
            [
                BinaryOperation(
                    Reference('n'),
                    '*',
                    FunctionCall(Reference('fac'), [
                        BinaryOperation(
                            Reference('n'),
                            '-',
                            Number(1)
                        )
                    ])
                )
            ]
        )
    ]))
    Read('n')
    Print(
        UnaryOperation('-', FunctionCall(Reference('fac'), [Reference('n')]))
    )


if __name__ == "__main__":
    pytest.main()
