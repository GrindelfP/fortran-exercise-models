#!/bin/bash
gfortran ode_solver.f90 models.f90 pursuit.f90 -o pursuit.exec
./pursuit.exec

gnuplot -p << EOF
    set multiplot layout 2,1  # Разделим экран на две части

    # График 1: Траектории
    set title "Adaptive Pursuit Curves (Step Doubling)"
    set xlabel "X"
    set ylabel "Y"
    set grid
    set yrange [0:10]
    plot for [i=0:9] 'pursuit.dat' index i using 1:2 with lines \
         title sprintf("k = %.1f", 0.6 + i*0.1) lw 2

    # График 2: Как менялся шаг h в зависимости от X
    set title "Step Size Behavior (h vs X)"
    set xlabel "X"
    set ylabel "Step Size |h|"
    set logscale y  # Логарифмическая шкала, так как h сильно меняется
    plot for [i=0:9] 'pursuit.dat' index i using 1:(abs(\$3)) with lines \
         notitle
    
    unset multiplot
EOF