#!/bin/bash

rm -f duffing_anim.dat duffing.exec
gfortran ode_solver.f90 duffing_model.f90 duffing_gif.f90 -o duffing.exec
./duffing.exec

echo "Generating GIF... this may take a minute."
gnuplot << EOF
    set terminal gif animate delay 3 size 600,600 optimize
    set output 'duffing_chaos.gif'

    stats 'duffing_anim.dat' using 2:3 name "S" nooutput
    
    set xrange [-2.5:2.5]
    set yrange [-2:2]
    set grid
    set xlabel "x"
    set ylabel "v"

    do for [i=1:int(S_records):10] {
        start_idx = (i < 300) ? 0 : i - 300
        set title sprintf("Duffing Chaos: Time Step %d / %d", i, int(S_records))
        plot 'duffing_anim.dat' every ::start_idx::i using 2:3 with lines lc rgb "blue" notitle
    }
EOF