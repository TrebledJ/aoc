from sympy import *

data = []
n, v = symbols('n v', integer=True)

with open('input.txt', 'r') as f:
    data = [*map(int, f.read().split(','))]

data[1] = n
data[2] = v

idx = 0
while data[idx] != 99:
    if idx != 0:
        if data[idx] == 1:
            data[data[idx+3]] = data[data[idx+1]] + data[data[idx+2]]
        elif data[idx] == 2:
            data[data[idx+3]] = data[data[idx+1]] * data[data[idx+2]]
    idx += 4

expr = data[0] - 19690720
# print(expr)

t = symbols('t')
s = diophantine(expr, param=t)
# print(s)

a, b = list(s)[0]
# print(a, b)

### METHOD 1: sympy ###
sol_n = solve([0 <= a, a < 100, 0 <= 19224076-216000*a, 19224076-216000*a < 100], a)
# print(sol_n)  # ~89
n = 89      # inference
v = 19224076 - 216000*n

print(n, v)

### METHOD 2: Brute Force ###
# a = 0
# b = 19224076
# while not ((0 <= a < 100) and (0 <= b < 100)):
#     a += 1
#     b -= 216000
#     print(a, b)   # 89, 76
