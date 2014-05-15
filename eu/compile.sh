#!/bin/sh
FILE=$1

raco make ${FILE} || exit 0

echo Dumping
racket -e "(require (submod (file \"${FILE}\") dump))" -- || exit 0
echo

echo Compiling
racket -e "(require (submod (file \"${FILE}\") compile))" -- ${FILE}.bc || exit 0
llvm-nm ${FILE}.bc
echo

echo Interpreting with racket
racket -e "(require (submod (file \"${FILE}\") main))" -- || exit 0
echo

echo Interpreting with lli
lli -O3 ${FILE}.bc
echo

echo Compiling and running
llc -O3 -filetype=obj ${FILE}.bc -o ${FILE}.o && \
nm ${FILE}.o && \
clang ${FILE}.o -o ${FILE}.exe && \
./${FILE}.exe
echo

echo Connecting to C
strip -N main ${FILE}.o -o ${FILE}.d.o
clang ${FILE}.d.o lib/cinterop.c -o lib/cinterop.exe && \
./lib/cinterop.exe
echo
