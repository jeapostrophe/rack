#!/bin/sh
FILE=$1

raco make ${FILE} || exit 0

echo Dumping
racket -e "(require (submod (file \"${FILE}\") dump))" -- || exit 0

echo Compiling
racket -e "(require (submod (file \"${FILE}\") compile))" -- ${FILE}.bc || exit 0

echo Interpreting with racket
racket -e "(require (submod (file \"${FILE}\") main))" -- || exit 0

echo Interpreting with lli
lli -O3 ${FILE}.bc

echo Compiling and running
llc -O3 -filetype=obj ${FILE}.bc -o ${FILE}.o && \
clang ${FILE}.o -o ${FILE}.exe && \
./${FILE}.exe
