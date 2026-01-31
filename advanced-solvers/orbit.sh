#!/bin/bash

gfortran ode_solver.f90 models.f90 orbit.f90 -o orbit.exec
./orbit.exec
gnuplot -p << EOF
    set multiplot layout 1,2 title "Satellite Orbit Comparison"

    set grid
    set size square
    set xrange [-1.5:1.5]
    set yrange [-1.5:1.5]
    set xlabel "X"
    set ylabel "Y"

    # --- RKDP45 ---
    set title "RKDP45"
    plot 'orbit.dat' index 0 using 1:2 with lines lw 2 lc rgb "blue" title "RKDP45"

    # --- RK4 ---
    set title "RK4"
    plot 'orbit.dat' index 1 using 1:2 with lines lw 2 lc rgb "red" title "RK4"

    unset multiplot
EOF