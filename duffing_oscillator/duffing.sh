#!/bin/bash

gfortran ode_solver.f90 duffing_model.f90 duffing.f90 -o duffing.exec
./duffing.exec
gnuplot -p << EOF
    set multiplot layout 1, 2 title 'Duffing Oscillator'
    
    set grid
    set size square
    set xlabel 't'
    set ylabel 'x(t)'
    plot 'duffing.dat' using 1:2 with lines lc rgb 'blue' title 'x(t)'

    set grid
    set size square
    set xlabel 'x'
    set ylabel 'v(x)'
    plot 'duffing.dat' using 2:3 with lines lc rgb 'red' title 'v(x)'

    unset multiplot
EOF
