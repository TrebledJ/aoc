##  2 ms

import time as tm

with open('input.txt') as f:
    data = f.read().splitlines()
start = tm.time()

res = sum(map(int,data))

end = tm.time()
print('Result:', res)
print('Time Taken:', (end - start) * 1000, 'ms')
    
