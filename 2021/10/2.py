with open('input.txt') as io:
    lines = [list(line) for line in io.read().splitlines()]
    incomplete = []
    completion = []
    for line in lines:
        chunks = []
        corrupted = []
        for character in line:
            if character in ['(', '[', '{', '<']:
                chunks.append(character)
            else:
                closing = chunks.pop()
                if closing != {')': '(', ']': '[', '}': '{', '>': '<'}.pop(character):
                    corrupted.append(character)
                    break
        if not corrupted:
            total = 0
            for chunk in reversed(chunks):
                total = total * 5 + {'(': 1, '[': 2, '{': 3, '<': 4}.pop(chunk)
            completion.append(total)
    completion.sort()
    print(completion[int(len(completion) / 2)])
