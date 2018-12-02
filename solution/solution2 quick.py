##  200 ms

import time as tm

with open('../input/input2.txt') as f:
    data = list(map(int,f.read().splitlines()))
##data=[1000000, -999999]   ## 2 s
start = tm.time()

store={0: 1}

index = freq = 0
it = 0
while 1:
    if index >= len(data):
        index %= len(data)
    freq += data[index]
    if store.setdefault(freq, 0) == 1:
        break
    store[freq] += 1
    index += 1

end = tm.time()
print('Result:', freq, '\n')
print('Time Taken:', (end - start) * 1000, 'ms')
