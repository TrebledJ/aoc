
with open('/Users/Technist/Desktop/aoc/input/input15.txt', 'r') as f:
    data = f.read().splitlines()

# get first line
data = list(map(int, data[0].split()))

stack = []

READ_QUANTITY_CHILD = 0
READ_QUANTITY_META = 1
READ_CHILD = 2
READ_META = 3


nodes = {}

index = 0
state = READ_QUANTITY_CHILD

anId = 0

def numChildren(aId):
    return len(nodes[aId]['children'])

def maxChildren(aId):
    return nodes[aId]['qChildren']

def numMeta(aId):
    return len(nodes[stack[-1]]['meta'])

def maxMeta(aId):
    return nodes[stack[-1]]['qMeta']

while index < len(data):
    info = data[index]
##    print(info, end=' ')
    
    if state == READ_QUANTITY_CHILD:
##        print('Reading Qty Child')
        if stack:
            while maxChildren(stack[-1]) == numChildren(stack[-1]):
                last = stack.pop()
##                print('  Popping', last) 
                
            
            nodes[stack[-1]]['children'].append(anId)

##        print('  Creating Node', anId)
        nodes[anId] = ({
##                'parent': (-1 if not stack else nodes[stack[-1]]),
                'qChildren': info,
                'children': [],
                'qMeta': 0,
                'meta': [],
                'value': None
            })
        stack.append(anId)
        
        anId += 1

        
        
        state = READ_QUANTITY_META

        
    elif state == READ_QUANTITY_META:
##        print('Reading Qty Meta')
        nodes[stack[-1]]['qMeta'] = info
        
        if maxChildren(stack[-1]) == numChildren(stack[-1]):
            state = READ_META
        else:
            state = READ_QUANTITY_CHILD
        
        
    elif state == READ_META:
##        print('Reading Meta')

        nodes[stack[-1]]['meta'].append(info)
        
        # pop child if meta is all read
        if numMeta(stack[-1]) == maxMeta(stack[-1]):
            last = stack.pop()
##            print('  Popped', last)

        # parent
        if stack and numChildren(stack[-1]) != maxChildren(stack[-1]):
            state = READ_QUANTITY_CHILD
        else:
            state = READ_META

##    print('\t', 'next state:', state)
##    print('\t', 'stack:', stack)
##    if stack:
##        print('\t', 'child:', nodes[stack[-1]])
    
    index += 1

print('\n')
print('Done')

def calc(aId):
##    print('Calculating', aId)
    
    if aId not in nodes:
        return 0
    
    if nodes[aId]['value'] is not None:
        return nodes[aId]['value']
    
    if maxChildren(aId) == 0:
        nodes[aId]['value'] = sum(nodes[aId]['meta'])
##        print(' No Children; Sum Meta:', nodes[aId]['value'])
    else:
        nodes[aId]['value'] = sum(calc(nodes[aId]['children'][m - 1]) for m in nodes[aId]['meta'] if 0 <= m - 1 < maxChildren(aId))
##        print(' Sum Beneath:', nodes[aId]['value'])

    return nodes[aId]['value']

print('Root Value:', calc(0))





