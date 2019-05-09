
##import math
import numpy as np
##import pandas as pd

##with open('/Users/JLAW/Desktop/aoc/2018/d9/input.txt', 'r') as f:
##    data = f.read().splitlines()

players = 438
marbles = 7162600


##players = 30
##marbles = 5807
##marbles = 71626


mar = np.array([0, 1])
pos = 1

score = {}

for i in np.arange(2, marbles + 1):

    if i % 50000 == 0:
        print('Checkpoint:', i)
    
    if i % 23 == 0:
        pos = (pos - 7) % len(mar)

        player = i % players
        score.setdefault(player, 0)
        score[player] += i + mar[pos]

##        print('Removing', mar[pos])
        mar = np.delete(mar, pos)
        continue

    pos = (pos + 2) % len(mar)
    if pos == 0: pos = len(mar)
    mar = np.insert(mar, pos, i)
    
    pass


##print(mar)

print('\n\n')
print('Maximum:', max(score.values()))
