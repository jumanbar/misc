# -*- coding: utf-8 -*-
"""
Created on Thu May 22 14:44:31 2014

@author: Juan Manuel Barreneche

Proyecto: torres de Hanoi.

TowerSet:
    La clase TowerSet es una representación de las torres de Hanoi.
    Un TowerSet T es un set de 3 torres de Hanoi de n piezas.
    - Una instancia nueva tiene todas las piezas en la primer torre,
      ordenadas de mayor (abajo) a menor (arriba). Cada 'pieza' es un número.
    - El método move sirve para mover la pieza de más arriba de una torre a otra.
      Se puede usar de varias formas. Ejemplos (mover pieza de torre 1 a torre 2):
      >> T.move(12)
      >> T.move(1,2)
      >> T.move('12')
      >> T.move('1','2')

solve:
    La función solve mueve las piezas siguiendo la solución más eficiente (2^n - 1).
    En cada paso mueve una pieza dentro del TowerSet t.
    Es un problema recursivo, por lo que la función también lo es. Aplica la solución básica
    del problema con 2 piezas y la repite para torres de n piezas.
    Con 2 piezas la secuencia es:
      1) pieza 1: t.inicial     ---> t.alternativa
      1) pieza 2: t.inicial     ---> t.final
      1) pieza 1: t.alternativa ---> t.final
    Esta secuencia se repite siempre, cambiando "pieza 1" por todas las piezas menos la del fondo y
    "pieza 2" por la pieza mayor. La diferencia surge de que cada vez que se mueve la "pieza 1"
    de una torre a otra, ya no es un solo paso, si no la cantidad correspondiente a una torre de n - 1
    piezas (2^(n - 1) - 1).
    Se puede ver que el problema se deconstruye en un problema de n - 1 piezas, que a su vez contiene 
    un problema de n - 2 piezas, y así, hasta llegar a 2 piezas.

hanoi:
    La función hanoi simplemente muestra un ejemplo con n piezas.
"""

import sys

class TowerSet:
    """ ej:
    T = TowerSet(n)
    """
    n_mov = 0
    def __init__(self, n):
        self.torres = {'t1': range(n, 0, -1), 't2': [], 't3': []}
        self.n = n
    def __repr__(self):
        out = 'Movimientos: ' + str(self.n_mov) + \
          '\nt1: ' + '-'.join([str(n) for n in self.torres['t1']]) + \
          '\nt2: ' + '-'.join([str(n) for n in self.torres['t2']]) + \
          '\nt3: ' + '-'.join([str(n) for n in self.torres['t3']])
        return out
    def __str__(self):
        return self.__repr__()
    def move(self, *args):
        if type(args[0]) == list:
            ini = 't' + str(args[0][0])
            fin = 't' + str(args[0][1])
        elif len(args) == 1:
            args = str(args[0])
            if len(args) < 2:
                print 'Error:'
                print 'El movimiento necesita 2 valores: origen y destino.'
                return 0 
            print 'aca', args
            ini = 't' + args[0]
            fin = 't' + args[1]
        elif len(args) >= 2:
            ini = 't' + str(args[0])
            fin = 't' + str(args[1])
        fini = self.torres[ini][-1]
        print str(fini) + ': ' + ini + ' ---> ' + fin
        if len(self.torres[fin]) > 0:
            ffin = self.torres[fin][-1]
        else:
            ffin = fini + 1
        if fini > ffin:
            print 'Error:'
            print 'La ficha ' + str(fini) + ' no se puede colocar encima de la ficha ' + str(ffin) + '.'
            print 'No se puede realizar el movimiento.'
            return 0
        self.torres[ini].remove(fini)
        self.torres[fin].append(fini)
        self.n_mov = self.n_mov + 1
        print self
        
def solve(t, n=-2, ini=1, fin=3):
    """ ej:
    T = TowerSet(3)
    solve(T)
    """
    if n == -2:
        n = t.n
    if t.n_mov == 0 and n == t.n:
        print 'Torres de Hanoi con ' + str(n) + ' piezas,'
        print 'debe resolverse en ' + str(2 ** n - 1) + ' movimientos:'
        print t
        print ' ==== Inicio ===='
    alt = [k for k in range(1,4) if k != ini and k != fin][0] # alt es el número de la tercera torre
    if n == 2:
        t.move(ini, alt)
        t.move(ini, fin)
        t.move(alt, fin)
    else:
        solve(t, n - 1, ini, alt)
        t.move(ini, fin)
        solve(t, n - 1, alt, fin)
    if t.n_mov == ((2 ** t.n) - 1) and n == t.n:
        print 'Ya se han hecho 2^' + str(t.n) + ' - 1 = ' + str(t.n_mov) + ' movimientos.'

class Steps:
    def __init__(self, n):
        self.step_list = [[0,0]] * (2 ** n - 1)
        self.i = 0
    def __repr__(self):
        return str(self.step_list)
    def __str__(self):
        return self.__repr__()
    def add(self, lst):
        self.step_list[self.i] = lst
        self.i = self.i + 1
    def __len__(self):
        return len(self.step_list)
    def __getitem__(self, i):
        return self.step_list[i]


def solve_steps(n, out = [], ini=1, fin=3, start=True):
    if start:
        out = Steps(n)
    alt = [k for k in range(1,4) if k != ini and k != fin][0] # alt es el número de la tercera torre
    if n == 2:
        out.add([ini, alt])
        out.add([ini, fin])
        out.add([alt, fin])
    else:
        solve_steps(n - 1, out, ini, alt, False)
        out.add([ini, fin])
        solve_steps(n - 1, out, alt, fin, False)
    return out


def hanoi(n):
    """ ej:
    hanoi(3) 
    """
    T = TowerSet(n)
    solve(T)

if __name__ == '__main__':
    print '[modulo hanoi importado]'
    if len(sys.argv) > 1:
        npiezas = int(sys.argv[1])
        hanoi(npiezas)

