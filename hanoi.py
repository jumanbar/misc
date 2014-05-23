# -*- coding: utf-8 -*-
"""
Created on Thu May 22 14:44:31 2014

@author: Juan Manuel Barreneche
"""

import sys

class TowerSet:
    n_mov = 0
    def __init__(self, n):
        self.torres = {'t1': range(n, 0, -1), 't2': [], 't3': []}
        self.update_ts()
        self.n = n
    def update_ts(self):
        self.t1 = self.torres['t1']
        self.t2 = self.torres['t2']
        self.t3 = self.torres['t3']
    def show(self):
        #print '#Piezas: ' + str(self.n)
        print 'Movimientos: ' + str(self.n_mov)
        print 't1: ' + '-'.join([str(n) for n in self.t1])
        print 't2: ' + '-'.join([str(n) for n in self.t2])
        print 't3: ' + '-'.join([str(n) for n in self.t3])
    def move(self, ini, fin):
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
        self.update_ts()
        self.n_mov = self.n_mov + 1
        self.show()
        print ''
        
def solve(t, n=-2, ini='t1', fin='t3'):
    if n == -2:
        n = t.n
    if t.n_mov == 0 and n == t.n:
        print 'Torres de Hanoi con ' + str(n) + ' piezas,'
        print 'debe resolverse en ' + str(2 ** n - 1) + ' movimientos:'
        t.show()
        print ' ==== Inicio ===='
    alt = [k for k in t.torres.keys() if k != ini and k != fin][0]
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

def hanoi(n):
    T = TowerSet(n)
    solve(T)

if __name__ == '__main__':
    print '[modulo hanoi importado]'
    if len(sys.argv) > 1:
        npiezas = int(sys.argv[1])
    else:
        npiezas = 3
    hanoi(npiezas)

