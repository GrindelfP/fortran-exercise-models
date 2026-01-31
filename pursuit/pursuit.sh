#!/bin/bash

gfortran pursuit.f90 -o pursuit.exec
./pursuit.exec

gnuplot -p << EOF
    set title "Trader vs Pirate - Pursuit Problem"
    set xlabel "X"
    set ylabel "Y"
    set grid

    set yrange [0:10]

    k_start = 0.6
    k_step = 0.1

    plot for [i=2:11] 'pursuit.dat' using 1:i with lines \
         title sprintf("k = %.1f", k_start + (i-2)*k_step) lw 2
EOF