// modeled after
// http://norvig.com/lispy.html

#![warn(clippy::all)]

// GDB on macOS issue
//https://youtrack.jetbrains.com/issue/CPP-14546

// https://users.rust-lang.org/t/takewhile-iterator-over-chars-to-string-slice/11014/2

use std::rc::Rc;

use scheme_lib::{list, run, test_eq, Expression};

fn main() {
    use Expression::{List, Number, SString};

    std::env::set_var("RUST_BACKTRACE", "full");
    color_backtrace::install();

    let input = "(define foo (lambda () \"a\")) (foo)";
    test_eq!(run(input) => SString("a".to_string()));

    let input = "(define a '(1 2 3)) (cdr a)";
    test_eq!(run(input) => list!(Number(2), Number(3)));
    // let input = "(define fib (lambda (n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 36)";
    // dbg!(run(input));
}
