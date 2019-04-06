#!/usr/bin/env python3
import pytest
from folder import *
from printer import *


def test_num_num():
    const_folder = ConstantFolder()
    check = const_folder.fold(BinaryOperation(Number(45), '-', Number(3)))
    assert check == Number(42)
    check = const_folder.fold(BinaryOperation(Number(7), '*', Number(6)))
    assert check == Number(42)


def test_num_ref():
    const_folder = ConstantFolder()
    check = const_folder.fold(BinaryOperation(Number(0),
                                              '*', Reference('foo')))
    assert check == Number(0)


def test_ref_num():
    const_folder = ConstantFolder()
    check = const_folder.fold(BinaryOperation(Reference('foo'),
                                              '*', Number(0)))
    assert check == Number(0)


def test_ref_ref():
    const_folder = ConstantFolder()
    check = const_folder.fold(BinaryOperation(Reference('foo'),
                                              '-', Reference('foo')))
    assert check == Number(0)


def test_num():
    const_folder = ConstantFolder()
    check = const_folder.fold(UnaryOperation('-', Number(42)))
    assert check == Number(-42)
    check = const_folder.fold(UnaryOperation('!', Number(42)))
    assert check == Number(0)


def test_all(capsys):
    pretty_print(fold_constants(
        BinaryOperation(
            Number(41),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    ),
                    '+',
                    Number(1)
                )
            )
        )
    ))
    assert capsys.readouterr().out == '42;\n'


if __name__ == "__main__":
    pytest.main()