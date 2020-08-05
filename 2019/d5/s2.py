import functools

data = []
params = {
    1: 3,
    2: 3,
    3: 1,
    4: 1,
    5: 2,
    6: 2,
    7: 3,
    8: 3,
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

    modified = False

    # if idx != 0:
    if opcode == 1:
        v1 = value(data[idx + 1], modes[0])
        v2 = value(data[idx + 2], modes[1])
        data[data[idx + 3]] = v1 + v2
    elif opcode == 2:
        v1 = value(data[idx + 1], modes[0])
        v2 = value(data[idx + 2], modes[1])
        data[data[idx + 3]] = v1 * v2
    elif opcode == 3:
        data[data[idx + 1]] = 5
    elif opcode == 4:
        val = str(value(data[idx + 1], modes[0]))
        output += val
    elif opcode == 5:
        if value(data[idx + 1], modes[0]) != 0:
            idx = value(data[idx + 2], modes[1])
            modified = True
    elif opcode == 6:
        if value(data[idx + 1], modes[0]) == 0:
            idx = value(data[idx + 2], modes[1])
            modified = True
    elif opcode == 7:
        data[data[idx + 3]] = [0, 1][value(data[idx + 1], modes[0]) < value(data[idx + 2], modes[1])]
    elif opcode == 8:
        data[data[idx + 3]] = [0, 1][value(data[idx + 1], modes[0]) == value(data[idx + 2], modes[1])]

    if not modified:
        idx += params[opcode] + 1

print(output)

