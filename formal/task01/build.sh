#!/bin/bash
if [ ! -d cmake-build-release ]
then
  mkdir cmake-build-release
fi
cd cmake-build-release

cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build  .

cd ..
