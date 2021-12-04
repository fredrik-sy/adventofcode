from io import open


def calculate(input, reversed=False):
    for x in range(len(input[0])):
        count = 0
        for y in range(len(input)):
            if input[y][x] == '0':
                count += 1
            else:
                count -= 1
        if count > 0:
            input = [numbers for numbers in input
                     if numbers[x] == ('1' if reversed else '0')]
        else:
            input = [numbers for numbers in input
                     if numbers[x] == ('0' if reversed else '1')]
        if len(input) == 1:
            return int(input.pop(), 2)
    raise Exception("Unknown diagnostic report")


with open('input.txt') as io:
    input = io.read()
    input = input.split('\n')
    oxygen_generator_rating = calculate(input)
    scrubber_rating = calculate(input, True)
    print(oxygen_generator_rating * scrubber_rating)
