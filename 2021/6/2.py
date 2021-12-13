with open('input.txt') as io:
    lanternfish = [0] * 9
    next_lanternfish = [0] * 9
    for fish in map(int, io.read().split(',')):
        lanternfish[fish] += 1
    for day in range(256):
        for i in range(len(lanternfish)):
            if i == 0:
                next_lanternfish[6] += lanternfish[0]
                next_lanternfish[8] += lanternfish[0]
            else:
                next_lanternfish[i - 1] += lanternfish[i]
        lanternfish = next_lanternfish
        next_lanternfish = [0] * 9
    print(sum(lanternfish))
