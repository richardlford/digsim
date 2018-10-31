  set terminal pdf font "Arial Italic, 12"
  set output "output.pdf"
  # define axis
  # remove border on top and right and set color to gray
  set style line 11 lc rgb '#808080' lt 1
  set border 3 back ls 11
  set tics nomirror
  # define grid
  set style line 12 lc rgb '#808080' lt 0 lw 1
  set grid back ls 12
  set nokey
  set zeroaxis lt 3 lw 2.5
  set title "Build09 3D Simplified Terminal Engagement Simulation"
set xlabel 'X_bi_i & X_ti_i (m)'
set y2label 'Z_bi_i & Z_ti_i (m)' rotate by 270
plot  "output.dat" using 2:4 with lines, "output.dat" using 5:7 with lines

set xlabel  'X_bi_i & X_ti_i (m)'
set y2label 'Y_bi_i & Y_ti_i (m)' rotate by 270
plot  "output.dat" using 2:3 with lines, "output.dat" using 5:6 with lines

set xlabel  'Time (sec)'
set y2label 'Xd_bi_i (m/sec)' rotate by 270
plot  "output.dat" using 1:8 with lines

set y2label 'Yd_bi_i (m/sec)' rotate by 270
plot  "output.dat" using 1:9 with lines
set y2label 'Zd_bi_i (m/sec)' rotate by 270
plot  "output.dat" using 1:10 with lines

set y2label 'Psi_b & Psi_b_Est (rad)' rotate by 270
plot  "output.dat" using 1:11 with lines, "output.dat" using 1:19 with lines

set y2label 'Theta_b & Theta_b_Est (rad)' rotate by 270
plot  "output.dat" using 1:12 with lines,  "output.dat" using 1:20 with lines

set y2label 'Phi_b & Phi_b_Est (rad)' rotate by 270
plot  "output.dat" using 1:13 with lines, "output.dat" using 1:21 with lines

set y2label 'P_b (rad/sec)' rotate by 270
plot  "output.dat" using 1:14 with lines
set y2label 'Q_b (rad/sec)' rotate by 270
plot  "output.dat" using 1:15 with lines
set y2label 'R_b (rad/sec)' rotate by 270
plot  "output.dat" using 1:16 with lines
set y2label 'Q_si_b_Meas (rad/sec)' rotate by 270
plot  "output.dat" using 1:17 with lines
set y2label 'R_si_b_Meas (rad/sec)' rotate by 270
plot  "output.dat" using 1:18 with lines
