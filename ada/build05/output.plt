set terminal pdf font "Arial Italic, 12"
set output "output.pdf"
# remove border on top and right and set color to gray
set style line 11 linecolor rgb 'black' linetype 1
set border 3 back linestyle 11
set tics nomirror
set style line 12 linecolor rgb 'black' lt 0 linewidth 1
set grid back linestyle 12
set nokey
set zeroaxis linetype 3 linewidth 2.5
set style line 1 linetype 1 linewidth 1 lc 'red'
set title "TIME vs. X"
set xlabel ""
set ylabel "Position of ball [m]"  rotate by 90
plot  "output.dat" using 1:2 with lines ls 1
set title "TIME vs. XD"
set xlabel ""
set ylabel "Velocity of ball [m/sec]"  rotate by 90
plot  "output.dat" using 1:3 with lines ls 1
