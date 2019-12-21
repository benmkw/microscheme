// modeled after
// http://norvig.com/lispy.html

#![warn(
    // clippy::restriction,
    // clippy::cargo
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::clone_on_ref_ptr
)]
#![allow(
    clippy::shadow_unrelated,
    clippy::cognitive_complexity,
    clippy::too_many_lines,
    clippy::use_debug,
    clippy::dbg_macro,
    clippy::print_stdout,
    clippy::missing_docs_in_private_items,
    clippy::implicit_return,
    clippy::option_unwrap_used
)]

// GDB on macOS issue
//https://youtrack.jetbrains.com/issue/CPP-14546

// https://users.rust-lang.org/t/takewhile-iterator-over-chars-to-string-slice/11014/2

use std::rc::Rc;

use scheme_lib::*;

fn main() {
    use Expression::*;

    std::env::set_var("RUST_BACKTRACE", "full");
    color_backtrace::install();

    let input = "(define foo (lambda () \"a\")) (foo)";
    test_eq!(run(input) => SString("a".to_string()));

    let input = "(define a '(1 2 3)) (cdr a)";
    test_eq!(run(input) => list!(Number(2), Number(3)));
}
