import math as m
import random as r

## ==================================
def int2bin (x):
  if x > 0:
    e = int(m.log(x, 2))
    x = x - 2 ** e
    return 10 ** e + int2bin(x)
  else:
    return 0

## ==================================
def get_parent (linea, pos):
  if pos > len(linea) - 1 or pos < 0:
    return 0
  pos = pos + 1
  if pos == 1:
    return linea[-1] + linea[0:2]
  elif pos == len(linea):
    return linea[-2:] + linea[0]
  else:
    return linea[(pos - 2):(pos + 1)]
## ==================================
## Automata elemental
## (http://en.wikipedia.org/wiki/Elementary_cellular_automaton)
## Reglas:
#### 111 110 101 100 011 010 001 000
#110  0   1   1   0   1   1   1   0
# 30  0   0   0   1   1   1   1   0
# 37  0   0   1   0   0   1   0   1
# 18  0   0   0   1   0   0   1   0
# 90  0   1   0   1   1   0   1   0
#150  1   0   0   1   0   1   1   0
# 15  0   0   0   0   1   1   1   1
# 45  0   0   1   0   1   1   0   1
#169  1   0   1   0   1   0   0   1
## Otras reglas interesantes:
## 9 - 22 - 26 - 41 - 54 - 57 - 60 - 62 - 73 - 104 - 105 - 106 - 122 - 126 - 146 - 154 - 184
def automata(regla, n = 20, width = 20):
  regla_bin = str(int2bin(regla))
  d = 8 - len(regla_bin)
  if d > 0:
    regla_bin = '0' * d + regla_bin

  keys = ['111', '110', '101', '100', '011', '010', '001', '000']
  ref = {x: regla_bin[i] for i,x in enumerate(keys)}

  linea = ''
  for i in range(width): linea = linea + str(r.randint(0, 1))
  print linea
  
  for t in range(n):
    nueva_linea = ''
    for i in range(width):
      parent = get_parent(linea, i)
      nueva_linea = nueva_linea + ref[parent]
    linea = nueva_linea
    print linea

if __name__ == '__main__':
  automata(args)
