#!/usr/bin/env python3
import pytest
from folder import *


def test_bin_operation1():
    assert fold_constants(BinaryOperation(Number(1), '+',
                                          Number(12))) == Number(13)


def test_bin_operation2():
    assert fold_constants(BinaryOperation(Reference('x'),
                                          '-', Reference('x'))) == Number(0)


def test_bin_operation3():
    assert fold_constants(BinaryOperation(Reference('x'),
                                          '*', Number(0))) == Number(0)


def test_bin_operation4():
    assert fold_constants(BinaryOperation(Number(0),
                                          '*', Reference('x'))) == Number(0)


def test_un_operation():
    unary_op = UnaryOperation('-', (UnaryOperation('-', Number(13))))
    assert fold_constants(unary_op) == Number(13)


def test_end_to_end():
    assert fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    ) == Number(13)


if __name__ == "__main__":
    pytest.main()
