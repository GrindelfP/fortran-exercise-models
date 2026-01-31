#!/bin/bash

gfortran ode_solver.f90 models.f90 josephson.f90 -o josephson.exec
./josephson.exec
gnuplot -p << EOF
    set multiplot layout 1,2 title "Josephsom junction Voltage and Phase over time"
    set grid
    set size square
    plot 'josephson.dat' using 1:2 with lines title "phi over time"
    plot 'josephson.dat' using 1:3 with lines title "V over time"
    unset multiplot
EOF