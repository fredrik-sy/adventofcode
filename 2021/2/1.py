from io import open

with open('input.txt') as io:
    input = io.read()
    input = input.split('\n')
    input = list(map(str.split, input))
    position = 0
    depth = 0
    for command, units in input:
        if command == 'forward':
            position += int(units)
        elif command == 'down':
            depth += int(units)
        elif command == 'up':
            depth -= int(units)
        else:
            raise Exception('Unknown command')
    print(position * depth)
