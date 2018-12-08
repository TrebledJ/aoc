##  140 ms

import sys
import time as tm
import numpy as np

##with open('../input/input5.txt') as f:
with open('/Users/JLAW/Desktop/aoc/input/input5.txt') as f:
    data = f.read().splitlines()
start = tm.time()



mx=my=mw=mh=0

sz = 1050

grid = np.matrix(
    [[0] * sz] * sz
)

found = None
for line in data:
    id_, _, coor, dim = line[1:].split()
    x, y = coor[:-1].split(',')
    w, h = dim.split('x')
    x, y, w, h = map(int, [x, y, w, h])

    grid[x:x+w, y:y+h] += 1
    
##    if len(np.where(grid[x:x+w, y:y+h] > 1)[0]) > 0:
##        found = id_
##        break

for line in data:
    id_, _, coor, dim = line[1:].split()
    x, y = coor[:-1].split(',')
    w, h = dim.split('x')
    x, y, w, h = map(int, [x, y, w, h])
##    print('Seek:')
##    print(grid[x:x+w, y:y+h])
##    print('np.where')
##    print(np.where(grid[x:x+w, y:y+h] != 1))
    if len(np.where(grid[x:x+w, y:y+h] != 1)[0]) == 0:
        found = id_
        break

end = tm.time()

print('ID:', '<none>' if found is None else found)


print('Time Taken:', (end - start) * 1000, 'ms')
