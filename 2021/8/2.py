from io import open
import numpy as np


def find(digits, length):
    l = list()
    for digit in digits:
        if len(digit) == length:
            l.append(digit)
    return l


def calculate(segments, digit):
    value = 0
    for segment in segments:
        value <<= 1
        if segment in digit:
            value |= 1
    return value


with open('input.txt') as io:
    sum = 0
    for line in io.read().splitlines():
        input, output = map(str.split, line.split(' | '))
        input = [set(sorted(digit)) for digit in input]
        output = [set(sorted(digit)) for digit in output]
        segments = [None] * 7
        segments[2], segments[5] = find(input, 2).pop()
        segments[0], = find(input, 3).pop() - set(segments)
        segments[1], segments[3] = find(input, 4).pop() - set(segments)
        segments[4], segments[6] = find(input, 7).pop() - set(segments)
        for digit in find(input, 6):
            index = segments.index((set(segments) - digit).pop())
            if index == 1:
                segments[1], segments[3] = segments[3], segments[1]
            elif index == 5:
                segments[2], segments[5] = segments[5], segments[2]
            elif index == 6:
                segments[4], segments[6] = segments[6], segments[4]
        total = ''
        for digit in output:
            value = calculate(segments, digit)
            if value == 127:
                total += '8'
            elif value == 91:
                total += '3'
            elif value == 123:
                total += '9'
            elif value == 58:
                total += '4'
            elif value == 82:
                total += '7'
            elif value == 18:
                total += '1'
            elif value == 119:
                total += '0'
            elif value == 93:
                total += '2'
            elif value == 107:
                total += '5'
            elif value == 111:
                total += '6'
        sum += int(total)
    print(sum)