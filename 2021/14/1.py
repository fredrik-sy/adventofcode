

from collections import Counter


class Polymer:
    def __init__(self, value) -> None:
        self.next = None  # type: Polymer | None
        self.value = value

    def __str__(self) -> str:
        return self.value

    def __repr__(self) -> str:
        return self.value


with open('input.txt') as io:
    formula, rules = io.read().split('\n\n')
    rules = dict(line.split(' -> ') for line in rules.splitlines())
    root_polymer = list(map(Polymer, formula))
    while len(root_polymer) > 1:
        root_polymer[-1].next = root_polymer.pop()
    root_polymer = root_polymer.pop()
    steps = 10
    while steps > 0:
        current_polymer = root_polymer
        while current_polymer.next:
            next_polymer = current_polymer.next
            new_polymer = Polymer(rules[str(current_polymer) + str(next_polymer)])
            new_polymer.next = next_polymer
            current_polymer.next = new_polymer
            current_polymer = next_polymer
        steps -= 1
    counter = Counter()
    while root_polymer:
        counter[str(root_polymer)] += 1
        root_polymer = root_polymer.next
    print(counter.most_common(1)[0][1] - counter.most_common()[-1][1])
