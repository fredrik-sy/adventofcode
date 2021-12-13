from collections import Counter

with open('input.txt') as io:
    formula, rules = io.read().split('\n\n')
    rules = dict(line.split(' -> ') for line in rules.splitlines())
    counter = Counter()
    for i in range(len(formula) - 1):
        counter[formula[i:i + 2]] += 1
    steps = 40
    while steps > 0:
        for polymer, count in list(counter.items()):
            if count > 0:
                counter[polymer] -= count
                counter[polymer[0] + rules[polymer]] += count
                counter[rules[polymer] + polymer[1]] += count
        steps -= 1
    sum = Counter()
    for polymer, count in list(counter.items()):
        if count > 0:
            sum[polymer[0]] += count
    sum[formula[-1]] += 1
    print(sum.most_common(1)[0][1] - sum.most_common()[-1][1])
