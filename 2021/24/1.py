def read():
    with open('input.txt') as io:
        instructions = list(map(str.split, io.read().splitlines()))
        for i in range(0, 252, 18):
            yield int(instructions[i + 5][2]), int(instructions[i + 15][2])


output = [0] * 14
stack = []
indices = []

for i, (a, b) in enumerate(read()):
    if a > 0:
        indices.append(i)
        stack.append(b)
    else:
        c = a + stack.pop()
        value = 9 if c < 0 else 9 - c
        output[indices.pop()] = value
        output[i] = value + c

print(''.join(map(str, output)))
