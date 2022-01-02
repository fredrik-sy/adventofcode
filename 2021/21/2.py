import re
from itertools import product
from functools import lru_cache, reduce


def add(wins, next_wins):
    return wins[0] + next_wins[0], wins[1] + next_wins[1]


def rolls(position, score):
    for rolls in product(range(1, 4), repeat=3):
        next_position = (position + sum(rolls) - 1) % 10 + 1
        next_score = score + next_position
        yield next_position, next_score


@lru_cache(None)
def play(player, position, score, opponent_position, opponent_score):
    if score >= 21:
        return 1, 0
    if opponent_score >= 21:
        return 0, 1

    if player:
        return reduce(add, [play(0, next_position, next_score, opponent_position, opponent_score) for next_position, next_score in rolls(position, score)])
    else:
        return reduce(add, [play(1, position, score, next_position, next_score) for next_position, next_score in rolls(opponent_position, opponent_score)])


with open('input.txt') as io:
    position_1, position_2 = (int(match.group(1)) for match in re.finditer(
        r'Player \d starting position: (\d)', io.read()))
    print(max(play(1, position_1, 0, position_2, 0)))
