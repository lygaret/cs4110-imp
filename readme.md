`imp`, a simple imperative language with conditionals stores.

see lecture 5 slides and notes for information:
https://www.cs.cornell.edu/courses/cs4110/2021fa/schedule.html

```
# example.imp

foo := 10;
bar := 3;

while bar < foo {
    print foo;
    print bar;

    bar := bar + bar
};

print bar;
```

```
$ imp.exe example.imp
[print foo = 10]
[print bar = 3]
[print foo = 10]
[print bar = 6]
[print bar = 12]

bar = 12
bar = 6
bar = 3
foo = 10
```

## run

1. have `menhir` installed with opam
1. build with `dune build`
1. test with `dune exec imp` (for stdin, or pass a file)

---

note that I'm not affiliated with Cornell in any way, I simply found the lecture notes and decided to implement the languages presented therein.