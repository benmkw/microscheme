# microscheme

![](https://github.com/benmkw/microscheme/workflows/CI/badge.svg)

This is modeled after [(How to Write a (Lisp) Interpreter (in Python))](https://norvig.com/lispy.html) and does not yet include [(An ((Even Better) Lisp) Interpreter (in Python))
](https://norvig.com/lispy2.html).

TOOD:
    - this project needs a (simple) GC, RC does not work for this
    - maybe use serde s expression backend for parsing or try nom
    - add optimizations/ extensions from second part of Norvig tutorial
    - tail calls
    - petgraph [dot file support](https://docs.rs/petgraph/0.5.1/petgraph/dot/struct.Dot.html) for visulisation

It is part of my attempt at learning more about rust and languages, see [lang](https://github.com/benmkw/lang)

It should be faster than the python interpreter but fib is about 5x slower than python so its still very slow.
