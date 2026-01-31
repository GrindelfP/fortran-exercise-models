#!/bin/bash

gfortran tomjerry.f90 -o tomjerry.exec
./tomjerry.exec

gnuplot -p << EOF
    set title "Tomcat and Mouse Problem"
    set xlabel "X"
    set ylabel "Y"
    set grid
    set size square

    set parametric
    set trange [0:2*pi]

    plot 'tomjerry.dat' using 1:2 with lines title 'Tomcat', \
        cos(t), sin(t) with lines title 'Mouse'
EOF