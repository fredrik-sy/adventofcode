from __future__ import annotations
from io import open


def lmap(func, iterables):
    return list(map(func, iterables))


def input():
    with open('input.txt') as io:
        return lmap(lambda v: list(reversed(v)), io.read().splitlines())


def snafu_to_decimal(snafu):
    counterparts = {'2': 2,
                    '1': 1,
                    '0': 0,
                    '-': -1,
                    '=': -2}
    decimal = 0

    for index, number in enumerate(snafu):
        decimal += counterparts[number] * (5 ** index)

    return decimal


def decimal_to_snafu(decimal):
    counterparts = {5: '0',
                    4: '-',
                    3: '=',
                    2: '2',
                    1: '1',
                    0: '0'}
    snafu = []
    rest = 0

    while decimal > 0:
        number = decimal % 5 + rest
        rest = 1 if number > 2 else 0
        decimal //= 5
        snafu.append(counterparts[number])

    return ''.join(reversed(snafu))


def part1():
    numbers = input()
    numbers_sum = 0

    for number in numbers:
        numbers_sum += snafu_to_decimal(number)

    print(f'Part 1: {decimal_to_snafu(numbers_sum)}')


if __name__ == '__main__':
    part1()
