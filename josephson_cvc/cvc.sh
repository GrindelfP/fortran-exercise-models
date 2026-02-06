#!/bin/bash
gfortran ode_solver.f90 josephson_model.f90 josephson_cvc.f90 -o cvc.exec
./cvc.exec
gnuplot -p << EOF
    set title "Josephson Junction CVC (IV Curve)" font ",14"
    set xlabel "Current (I / Ic)"
    set ylabel "Average Voltage (<V>)"
    set grid
    set key bottom right
    
    # Draw Shapiro steps lines if needed, or just standard grid
    set style line 1 lc rgb '#0060ad' lt 1 lw 2
    
    plot 'cvc_data.dat' using 1:2 with lines ls 1 title "RCSJ Model"
EOF
