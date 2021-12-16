class PacketDecoder:
    def __init__(self, packet):
        self.i = 0
        self.packet = packet
        self.sum = 0

    def read_binary(self, i):
        self.i += i
        return self.packet[self.i - i:self.i]

    def read_decimal(self, i):
        self.i += i
        return int(self.packet[self.i - i:self.i], 2)

    def parse(self):
        self.sum += self.read_decimal(3)
        if self.read_decimal(3) == 4:
            number = ''
            while self.read_decimal(1) == 1:
                number += self.read_binary(4)
            number += self.read_binary(4)
            return int(number, 2)
        else:
            if self.read_decimal(1) == 0:
                total = self.read_decimal(15)
                i = self.i
                while self.i - i < total:
                    self.parse()
            else:
                for _ in range(self.read_decimal(11)):
                    self.parse()


with open('input.txt') as io:
    decoder = PacketDecoder(''.join(bin(int(hexadecimal, 16))[2:].zfill(4) for hexadecimal in list(io.read())))
    decoder.parse()
    print(decoder.sum)
