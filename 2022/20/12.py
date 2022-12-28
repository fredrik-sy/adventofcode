from __future__ import annotations
from dataclasses import dataclass
from io import open


@dataclass
class Node:
    value: int
    previous: Node
    next: Node

    def __repr__(self) -> str:
        return f'Node(value: {self.value}, previous: {self.previous.value}, next: {self.next.value})'


def input():
    with open('input.txt') as io:
        nums = list(map(lambda value: Node(
            int(value), None, None), io.read().splitlines()))

        for i, num in enumerate(nums):
            num.previous = nums[i - 1]
            num.next = nums[(i + 1) % len(nums)]

        return nums


def mix(nums: list[Node]):
    for num in nums:
        value = abs(num.value) % (len(nums) - 1)

        if value == 0:
            continue

        num.previous.next = num.next
        num.next.previous = num.previous

        if num.value > 0:
            next_num = num

            for _ in range(value):
                next_num = next_num.next

            num.previous = next_num
            num.next = next_num.next

            next_num.next.previous = num
            next_num.next = num
        else:
            previous_num = num

            for _ in range(value):
                previous_num = previous_num.previous

            num.previous = previous_num.previous
            num.next = previous_num

            previous_num.previous.next = num
            previous_num.previous = num


def find(num: Node, offset):
    while offset > 0:
        num = num.next
        offset -= 1
    return num.value


def part1():
    nums = input()
    nums_lookup = {num.value: num for num in nums}
    mix(nums)
    print(
        f'Part 1: {sum(find(nums_lookup[0], n % len(nums)) for n in (1000, 2000, 3000))}')


def part2():
    nums = input()

    for num in nums:
        num.value *= 811589153

    nums_lookup = {num.value: num for num in nums}

    for _ in range(10):
        mix(nums)

    print(
        f'Part 2: {sum(find(nums_lookup[0], n % len(nums)) for n in (1000, 2000, 3000))}')


if __name__ == '__main__':
    part1()
    part2()
