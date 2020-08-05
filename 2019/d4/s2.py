def is_non_decreasing(s):
    return all(a <= b for a, b in zip(s[0:], s[1:]))

def has_double(s):
    s = '#' + s + '#'
    return any(b == c and a != b and c != d for a, b, c, d in zip(s[0:], s[1:], s[2:], s[3:]))

def valid(s):
    return is_non_decreasing(s) and has_double(s)

rng = "235741-706948"
low, high = map(int, rng.split('-'))
count = 0
for i in range(low, high + 1):
    if valid(str(i)):
        count += 1
    
print(count)