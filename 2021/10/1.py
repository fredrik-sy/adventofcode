with open('input.txt') as io:
    lines = [list(line) for line in io.read().splitlines()]
    chunks = []
    corrupted = []
    for line in lines:
        for character in line:
            if character in ['(', '[', '{', '<']:
                chunks.append(character)
            else:
                if chunks.pop() != {')': '(', ']': '[', '}': '{', '>': '<'}.pop(character):
                    corrupted.append(character)
                    break
    print(sum(map(lambda x: 3 if x == ')' else 57 if x == ']' else 1197 if x == '}' else 25137, corrupted)))
