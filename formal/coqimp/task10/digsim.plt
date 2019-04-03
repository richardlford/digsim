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

set xlabel  'X\_bi\_i & X\_ti\_i (m)'
set y2label 'Y\_bi\_i & Y\_ti\_i (m)' rotate by 270
plot  "output.dat" using 2:3 with lines, "output.dat" using 5:6 with lines

set y2label 'Z\_bi\_i & Z\_ti\_i (m)' rotate by 270
plot  "output.dat" using 2:4 with lines, "output.dat" using 5:7 with lines

set xlabel  'Time (sec)'
set y2label 'Xd\_bi\_i (m/sec)' rotate by 270
plot  "output.dat" using 1:8 with lines

set y2label 'Yd\_bi\_i (m/sec)' rotate by 270
plot  "output.dat" using 1:9 with lines

set y2label 'Zd\_bi\_i (m/sec)' rotate by 270
plot  "output.dat" using 1:10 with lines

set y2label 'Psi\_b & Psi\_b\_Est (rad)' rotate by 270
plot  "output.dat" using 1:11 with lines, "output.dat" using 1:19 with lines

set y2label 'Theta\_b & Theta\_b\_Est (rad)' rotate by 270
plot  "output.dat" using 1:12 with lines,  "output.dat" using 1:20 with lines

set y2label 'Phi\_b & Phi\_b\_Est (rad)' rotate by 270
plot  "output.dat" using 1:13 with lines, "output.dat" using 1:21 with lines

set y2label 'P\_b & P\_b\_Cmd (rad/sec)' rotate by 270
plot  "output.dat" using 1:14 with lines, "output.dat" using 1:22 with lines

set y2label 'Q\_b & Q\_b\_Cmd (rad/sec)' rotate by 270
plot  "output.dat" using 1:15 with lines, "output.dat" using 1:23 with lines

set y2label 'R\_b & R\_b\_Cmd (rad/sec)' rotate by 270
plot  "output.dat" using 1:16 with lines, "output.dat" using 1:24 with lines

set y2label 'Q\_si\_b\_Meas (rad/sec)' rotate by 270
plot  "output.dat" using 1:17 with lines
