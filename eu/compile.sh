#!/bin/sh
FILE=$1

raco make ${FILE} && \
racket -e "(require (submod (file \"${FILE}\") compile))" -- ${FILE}.bc || exit 0

echo Interpreting
lli -O3 ${FILE}.bc

echo Compiling and running
llc -O3 -filetype=obj ${FILE}.bc -o ${FILE}.o && \
clang ${FILE}.o -o ${FILE}.exe && \
./${FILE}.exe
