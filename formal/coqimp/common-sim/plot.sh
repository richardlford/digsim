#!/usr/bin/env bash
bash build.sh
./extraction_ml/digsim_driver.byte
gnuplot digsim.plt
