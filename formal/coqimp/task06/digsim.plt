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
  set title "Build06 Simplified Terminal Engagement Simulation"
  set xlabel "X (m)"
  set y2label 'Z (m)' rotate by 270
  plot  "output.dat" using 1:2 with lines
  set xlabel "Time (sec)"
  set y2label 'Xd (m/sec)' rotate by 270
  plot  "output.dat" using 1:3 with lines
  set y2label 'Z (m)' rotate by 270
  plot "output.dat" using 2:3 with lines
  set xlabel "Time (sec)"
  set y2label 'Xd (m/sec)' rotate by 270
  plot  "output.dat" using 1:5 with lines
  set y2label 'Zd (m/sec)' rotate by 270
  plot  "output.dat" using 1:6 with lines
  set y2label 'Theta (rad)' rotate by 270
  plot  "output.dat" using 1:4 with lines
  set y2label 'Q_s (rad/sec)' rotate by 270
  plot  "output.dat" using 1:7 with lines
