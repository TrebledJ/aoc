##  120 ms

import time as tm
import numpy as np

with open('input.txt') as f:
    data = f.read().splitlines()
start = tm.time()



mx=my=mw=mh=0

sz = 1050

grid = np.matrix(
    [[0] * sz] * sz
)

for line in data:
    id_, _, coor, dim = line[1:].split()
    x, y = coor[:-1].split(',')
    w, h = dim.split('x')
    x, y, w, h = map(int, [x, y, w, h])

    grid[x:x+w, y:y+h] += 1

end = tm.time()
    
print(len(np.where(grid > 1)[0]))


print('Time Taken:', (end - start) * 1000, 'ms')
