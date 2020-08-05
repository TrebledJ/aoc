import functools

data = []
params = {
    1: 3,
    2: 3,
    3: 1,
    4: 1,
    99: 0
}

with open('input.txt', 'r') as f:
    data = [*map(int, f.read().split(','))]

def get_value(data, pos, mode):
    if mode == '1':
        return pos
    elif mode == '0':
        return data[pos]

value = functools.partial(get_value, data)

idx = 0
output = ''
while data[idx] != 99:
    code = data[idx]
    
    opcode = code % 100
    modes = ('000' + str(code // 100))[::-1]

    if opcode == 1:
        data[data[idx + 3]] = value(data[idx + 1], modes[0]) + value(data[idx + 2], modes[1])
    elif opcode == 2:
        data[data[idx + 3]] = value(data[idx + 1], modes[0]) * value(data[idx + 2], modes[1])
    elif opcode == 3:
        data[data[idx + 1]] = 1
    elif opcode == 4:
        output += str(value(data[idx + 1], modes[0]))

    idx += params[opcode] + 1

print(int(output))

