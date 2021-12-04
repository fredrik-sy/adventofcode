from io import open

with open('input.txt') as io:
    input = io.read()
    input = input.split('\n')
    input = list(map(str.split, input))
    position = 0
    depth = 0
    aim = 0
    for command, units in input:
        if command == 'forward':
            position += int(units)
            depth += aim * int(units)
        elif command == 'down':
            aim += int(units)
        elif command == 'up':
            aim -= int(units)
        else:
            raise Exception('Unknown command')
    print(position * depth)
