#!/usr/bin/bash

cargo build --release
cargo run --release > output.dat
gnuplot digsim.plt
../../comparedat.py

