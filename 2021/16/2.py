from functools import reduce
from operator import mul


class PacketDecoder:
    def __init__(self, packet):
        self.i = 0
        self.packet = packet
        self.values = []

    def read_binary(self, i):
        self.i += i
        return self.packet[self.i - i:self.i]

    def read_decimal(self, i):
        self.i += i
        return int(self.packet[self.i - i:self.i], 2)

    def parse_sub(self):
        total = self.read_decimal(15)
        i = self.i
        while self.i - i < total:
            yield self.parse()

    def parse(self):
        self.read_decimal(3)
        type = self.read_decimal(3)
        if type == 4:
            number = ''
            while self.read_decimal(1) == 1:
                number += self.read_binary(4)
            number += self.read_binary(4)
            return int(number, 2)
        else:
            if self.read_decimal(1) == 0:
                result = list(self.parse_sub())
            else:
                result = [self.parse() for _ in range(self.read_decimal(11))]
            if type == 0:
                return sum(result)
            elif type == 1:
                return reduce(mul, result, 1)
            elif type == 2:
                return min(result)
            elif type == 3:
                return max(result)
            elif type == 5:
                return 1 if result[0] > result[1] else 0
            elif type == 6:
                return 1 if result[0] < result[1] else 0
            elif type == 7:
                return 1 if result[0] == result[1] else 0
            else:
                return result


with open('input.txt') as io:
    decoder = PacketDecoder(''.join(bin(int(hexadecimal, 16))[2:].zfill(4) for hexadecimal in list(io.read())))
    print(decoder.parse())
