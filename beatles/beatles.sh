#!/bin/bash

gfortran beatles.f90 -o beatles.exec
./beatles.exec

gnuplot -p << EOF
    set title "Four Beatles - Pursuit Problem"
    set size square
    set grid
    plot 'beatles.dat' using 1:2 with lines title 'Bug 1', \
         'beatles.dat' using (-\$2):1 with lines title 'Bug 2', \
         'beatles.dat' using (-\$1):(-\$2) with lines title 'Bug 3', \
         'beatles.dat' using 2:(-\$1) with lines title 'Bug 4'
EOF