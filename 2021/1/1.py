from io import open

with open('input.txt') as io:
    input = io.read()
    input = input.split('\n')
    input = list(map(int, input))
    count = 0
    for i in range(1, len(input)):
        if input[i - 1] < input[i]:
            count += 1
    print(count)
