import numpy as np

with open('/Users/JLAW/Desktop/aoc/input/input11.txt', 'r') as f:
    data = f.read().splitlines()

data = list(map(lambda s: tuple(map(int, s.split(', '))), data))

sz = 1000
grid = np.matrix([[0]*sz] * sz)

def man(tup1, tup2):
    return abs(tup1[0] - tup2[0]) + abs(tup1[1] - tup2[1])

def compGrid(x, y):
    # tup will contain (index, coordinate)
    return min(list(enumerate(data)), key=lambda tup: man(tup[1], (x,y)))

store = {}

for i in range(sz):
    for j in range(sz):
        index, coor = compGrid(i, j)
        grid[i,j] = index
        store.setdefault(index, 0)
        store[index] += 1

isInfinite = {index for i in range(sz) for index in (grid[i, 0], grid[0, i], grid[i, sz - 1], grid[sz - 1, i])}
print(isInfinite)

def compare(index):
    if index in isInfinite: return -1
    return store[index]

index = max(store, key=lambda x: compare(x))
print(store[index])


