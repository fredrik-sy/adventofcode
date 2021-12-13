with open('input.txt') as io:
    fishes = list(map(int, io.read().split(',')))
    for day in range(80):
        for i in reversed(range(len(fishes))):
            if fishes[i] == 0:
                fishes[i] = 6
                fishes.append(8)
            else:
                fishes[i] -= 1
    print(len(fishes))
