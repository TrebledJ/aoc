import numpy as np

with open('input.txt', 'r') as f:
    data = f.read().splitlines()

data = list(map(lambda s: tuple(map(int, s.split(', '))), data))

sz = 1000

def man(tup1, tup2):
    return abs(tup1[0] - tup2[0]) + abs(tup1[1] - tup2[1])

numCoors = sum(1 for i in range(sz) for j in range(sz) if sum(man(coor, (i, j)) for coor in data) < 10000)

print(numCoors)


