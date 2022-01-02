import re


class DeterministicDie:
    def __init__(self) -> None:
        self.value = 0
        self.rolled = 0

    def roll(self):
        self.value = (self.value - 1) % 100 + 2
        self.rolled += 1
        return self.value


class Player:
    def __init__(self, position) -> None:
        self.position = position
        self.score = 0

    def roll(self, die):
        position = self.position + die.roll() + die.roll() + die.roll() - 1
        self.position = position % 10 + 1
        self.score += self.position


class Game:
    def __init__(self, player_1: Player, player_2: Player) -> None:
        self.player_1 = player_1
        self.player_2 = player_2
        self.die = DeterministicDie()

    def play(self):
        while True:
            self.player_1.roll(self.die)

            if self.player_1.score >= 1000:
                print(self.player_2.score * self.die.rolled)
                break

            self.player_2.roll(self.die)

            if self.player_2.score >= 1000:
                print(self.player_1.score * self.die.rolled)
                break


with open('input.txt') as io:
    player_1, player_2 = (Player(int(match.group(1))) for match in re.finditer(
        r'Player \d starting position: (\d)', io.read()))
    game = Game(player_1, player_2)
    game.play()
