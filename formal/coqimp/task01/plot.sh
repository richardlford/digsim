#!/usr/bin/env bash
coqc -Q . Task scndordr.v | perl -ne '$in_header = 1 .. m#Start of data#i;$in_tail = m#End of data#i .. eof(); print if !$in_header && !$in_tail'  > output.dat
gnuplot digsim.plt
