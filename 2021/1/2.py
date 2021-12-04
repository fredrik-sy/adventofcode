from io import open

with open('input.txt') as io:
    input = io.read()
    input = input.split('\n')
    input = list(map(int, input))
    count = 0
    for i in range(1, len(input) - 2):
        if sum(input[i - 1:i + 2]) < sum(input[i:i + 3]):
            count += 1
    print(count)
