##  50 s

import time as tm
import numpy as np

with open('input.txt') as f:
    data = list(map(int,f.read().splitlines()))
start = tm.time()

n=np.array(data)
states = np.array([])
index = 0
while 0 not in states:

    states = np.append(states, 0)
    states += n[index]
    
    index += 1

    if index == len(n):
        n = np.append(n, data)


index = np.where(states==0)[0][0]

end = tm.time()
print('Result:', np.sum(n[0:index]), '\n')
print('Time Taken:', (end - start), 's')
