from io import open

with open('input.txt') as io:
    times = 0
    for line in io.read().splitlines():
        input, output = map(str.split, line.split(' | '))
        input = [tuple(sorted(digit)) for digit in input]
        output = [tuple(sorted(digit)) for digit in output]

        for digit in output:
            if len(digit) in (2, 3, 4, 7):
                times += 1
    print(times)
