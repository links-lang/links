set terminal pngcairo size 700,500 enhanced font 'Verdana,11'
set output 'matmult.png'

#set logscale x

# Color definitions
set border linewidth 1.5
set style line 1 lc rgb '#dd181f' lt 1 lw 1 # --- red
set style line 2 lc rgb '#0060ad' lt 1 lw 1 # --- blue
set style line 3 lc rgb '#00B339' lt 1 lw 1 # --- green

# Axes
set style line 11 lc rgb '#808080' lt 1
set border 3 back ls 11
set tics nomirror

set xlabel "# (/ 1000)"
set xrange [1:1000]
set ylabel "Time (s)"
set yrange [*:6]

plot "sumlist.csv" u 1:2 w line ls 1 lw 1 t "Interpreted", \
	  "sumlist.csv" u 1:3 w line ls 2 lw 1 t "Compiled - no Doubleling", \
	  "sumlist.csv" u 1:4 w line ls 3 lw 1 t "Compiled - Doubleling"

