#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./zig-out/bin/chibicc.zig "$input" > ./zig-out/tmp.s || exit
  gcc -static -o ./zig-out/tmp ./zig-out/tmp.s
  ./zig-out/tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 0
assert 42 42

echo OK
