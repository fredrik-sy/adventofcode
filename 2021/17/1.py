import re


with open('input.txt') as io:
    match = re.match(r'target area: x=([0-9-]+)..([0-9-]+), y=([0-9-]+)..([0-9-]+)', io.read())
    minx, maxx, miny, maxy = map(int, match.groups())
    highest = set()
    for y in range(miny, abs(miny) + 1, -1 if miny > 0 else 1):
        for x in range(maxx + 1):
            probex = 0
            probey = 0
            record = []
            vy = y
            vx = x
            while probey >= miny and probex <= maxx:
                probex += vx
                probey += vy
                record.append(probey)
                vx = max(0, vx - 1)
                vy = vy - 1
                if miny <= probey <= maxy and minx <= probex <= maxx:
                    highest.add(max(record))
                    break
    print(max(highest))
