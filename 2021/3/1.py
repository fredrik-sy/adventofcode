from io import open

with open('input.txt') as io:
    input = io.read()
    input = input.split('\n')
    gamma_rate = ''
    epsilon_rate = ''
    for x in range(len(input[0])):
        count = 0
        for y in range(len(input)):
            if input[y][x] == '0':
                count += 1
            else:
                count -= 1
        if count > 0:
            gamma_rate += '0'
            epsilon_rate += '1'
        elif count < 0:
            gamma_rate += '1'
            epsilon_rate += '0'
        else:
            raise Exception("Count is zero")
    print(int(gamma_rate, 2) * int(epsilon_rate, 2))
