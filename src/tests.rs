#![allow(clippy::enum_glob_use)]

use super::*;
use Expression::*;

#[test]
fn basics() {
    let input = "(+ 3 4)";
    test_eq!(run(input) => Number(7));

    let input = "(+ 3 (* 3 2))";
    test_eq!(run(input) => Number(9));

    let input = "(- 6 2)";
    test_eq!(run(input) => Number(4));

    let input = "(* 3 (/ 4 2))";
    test_eq!(run(input) => Number(6));

    let input = "(* (- 3 1) (/ 4 2))";
    test_eq!(run(input) => Number(4));

    let input = "(if 100 (* 3 4) (+ 1 1))";
    test_eq!(run(input) => Number(12));

    let input = "(if #f (* 3 (* 2 2)) (+ 1 1))";
    test_eq!(run(input) => Number(2));

    let input = "(define a 2) (* a 3)";
    test_eq!(run(input) => Number(6));
}

#[test]
fn quote() {
    let input = "(quote (1 2 3 4 5))";
    test_eq!(
        run(input) =>
        List(vec![
            Rc::new(Number(1)),
            Rc::new(Number(2)),
            Rc::new(Number(3)),
            Rc::new(Number(4)),
            Rc::new(Number(5))
        ])
    );

    let input = "(quote (+ 2 3))";
    test_eq!(
        run(input) =>
        List(vec![
            Rc::new(Add),
            Rc::new(Number(2)),
            Rc::new(Number(3)),
        ])
    );

    let input = "'(+ 2 3)";
    test_eq!(
        run(input) =>
        List(vec![
            Rc::new(Add),
            Rc::new(Number(2)),
            Rc::new(Number(3)),
        ])
    );

    let input = "'2";
    test_eq!(run(input) => Number(2));

    let input = "'a";
    test_eq!(run(input) => Symbol("a".to_string()));
}

#[test]
fn functions() {
    let input = "(define r 8) (define p (+ 2 1)) (* r p)";
    test_eq!(run(input) => Number(24));

    let input = "(define r_val (if #f 0 10)) (define p (+ 2 1)) (* r_val p) (define unused_val 10)";
    test_eq!(run(input) => Number(30));

    let input = "(define twice (lambda (x) (* 2 x)) ) (twice 5)";
    test_eq!(run(input) => Number(10));

    let input = "(define a 2) (define twice (lambda (x) (* 2 x)) ) (set! a 500) (twice a)";
    test_eq!(run(input) => Number(1000));

    let input = "(define identity (lambda (x) x) ) (identity 5)";
    test_eq!(run(input) => Number(5));

    let input = "(define value_fn (lambda () (50)) ) (value_fn)";
    test_eq!(run(input) => Number(50));

    let input = "(define fib (lambda (n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 6)";
    test_eq!(run(input) => Number(8));

    // direct lambda
    let input = "((lambda (x) (+ x 3)) 7)";
    test_eq!(run(input) => Number(10));
}

#[test]
fn if_tests() {
    let input = "(< 2 1)";
    test_eq!(run(input) => ExprFalse);

    let input = "(> 1 2)";
    test_eq!(run(input) => ExprFalse);

    let input = "(= 2 1)";
    test_eq!(run(input) => ExprFalse);

    let input = "(define value (if (> 1 2) (10) (100) )) (* 5 value) ";
    test_eq!(run(input) => Number(500));

    let input = "(define value (if (> 2 1) (10) (100) )) (* 5 value) ";
    test_eq!(run(input) => Number(50));

    let input = "(define value (if (< 1 2) (10) (100) )) (* 5 value) ";
    test_eq!(run(input) => Number(50));

    let input = "(define value (if (< 2 1) (10) (100) )) (* 5 value) ";
    test_eq!(run(input) => Number(500));
}

#[test]
fn list_cons_car_cdr() {
    let input = "(car '(a b c))";
    test_eq!(run(input) => Symbol("a".to_string()));

    let input = "(cdr '(a b c))";
    test_eq!(run(input) => list!(Symbol("b".to_string()), Symbol("c".to_string())));

    let input = "(cons (quote a) '(b c))";
    test_eq!(run(input) => list!(Symbol("a".to_string()),list!(Symbol("b".to_string()), Symbol("c".to_string()))));

    let input = "(cons (quote a) (quote b))";
    test_eq!(run(input) => list!(Symbol("a".to_string()), Symbol("b".to_string())));

    let input = "(cons  'a '(b c))";
    test_eq!(run(input) => list!(Symbol("a".to_string()), list!(Symbol("b".to_string()),Symbol("c".to_string()))));

    let input = "(cons '(b c) 'a)";
    test_eq!(run(input) => list!(list!(Symbol("b".to_string()),Symbol("c".to_string())),Symbol("a".to_string())));

    let input = "(cons  '(a b) '(c d))";
    test_eq!(
        run(input) =>
        list!(
            list!(
                Symbol("a".to_string()),
                Symbol("b".to_string())),
            list!(
                Symbol("c".to_string()),
                Symbol("d".to_string())))
    );

    let input = "(cons 'a (cons 'b (cons 'c '())))";
    test_eq!(run(input) => list!(Symbol("a".to_string()), list!(Symbol("b".to_string()), list!(Symbol("c".to_string())))));

    let input = "(cdr '(a))";
    test_eq!(run(input) => list!());

    let input = "(cons 'a '())";
    test_eq!(run(input) => list!(Symbol("a".to_string())));

    let input = "(cons '() 'a)";
    test_eq!(run(input) => list!(list!(), Symbol("a".to_string())));

    let input = "(define a '(1 2 3)) (cdr a)";
    test_eq!(run(input) => list!(Number(2), Number(3)));

    let input = "(cons 'a '())";
    test_eq!(run(input) => list!(Symbol("a".to_string())));

    let input = "(cons '() 'a)";
    test_eq!(run(input) => list!(list!(), Symbol("a".to_string())));

    let input = "(cons '() '())";
    test_eq!(run(input) => list!(list!()));
}

#[test]
fn ref_semantic() {
    let input = "(define value 3) (define f (lambda () value)) (set! value 30) (f) ";
    test_eq!(run(input) => Number(30));
}

#[test]
fn re_defines_of_fn() {
    let input = "(define f +) (define x 3) (define y 2) (f x y)";
    test_eq!(run(input) => Number(5));

    let input = "(define gt >) (gt 1 2)";
    test_eq!(run(input) => ExprFalse);

    let input = "(define first car) (first '(a b c))";
    test_eq!(run(input) => Symbol("a".to_string()));

    let input = "(define add cons) (add (quote a) '(b c))";
    test_eq!(run(input) => list!(Symbol("a".to_string()),list!(Symbol("b".to_string()), Symbol("c".to_string()))));
}

#[test]
fn let_bindings() {
    let input = "(let ((x 5) (y 1)) (+ x y))";
    let input2 = "(define x 5) (define y 1) (+ x y)";
    assert_eq!(run(input), run(input2));
    test_eq!(run(input) => Number(6));

    // // what is the right semantic here? do we no honor the environments locally enough?
    // let input = "(let ((x 5))
    //                 (let ((x 2)
    //                       (y x))
    //                   (cons y x)))";
    // test_eq!(run(input) => list!(list!(Number(5)), Number(2)));
}

#[test]
fn equal_on_list() {
    let input = "(= '(1 2 3) '(1 2 3))";
    test_eq!(run(input) => Number(121_212));

    let input = "(= '(1 2 3) '(1 3 3))";
    test_eq!(run(input) => ExprFalse);

    let input = "(= '(1 2 3) '(1 2))";
    test_eq!(run(input) => ExprFalse);
}

#[test]
fn real_functions() {
    let input = "(define twice (lambda (x) (* 2 x))) (define repeat (lambda (f) (lambda (x) (f (f x))))) ((repeat twice) 10)";
    test_eq!(run(input) => Number(40));

    let input = "(define fac (lambda (x) (if (= x 0) 1 (* x (fac (- x 1)))))) (fac 10)";
    test_eq!(run(input) => Number(3_628_800));

    let input = "(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b))))) (range 0 5)";
    test_eq!(run(input) => list!(Number(0), list!(Number(1), list!(Number(2), list!(Number(3), list!(Number(4)))))));

    let input = "(define max (lambda (x)
                      (if (= (cdr x) '())
                        (car x)
                        (if (> (car x) (max (cdr x)))
                            (car x)
                            (max (cdr x))))))

                    (max '(1 2 30 4))";
    test_eq!(run(input) => Number(30));

    // TODO handle 0 case, maybe need to add NIL value for list
    // or just remove the check for adding empty list and use that
    let input = "(define length (lambda (list) (if (= (cdr list) '())
                                1
                                (+ 1 (length (cdr list))))))
                (length '(1 2 3 4))";
    test_eq!(run(input) => Number(4));
}

#[test]
fn strings() {
    let input = "(define s \"car\") (if #F 1 s)";
    test_eq!(run(input) => SString("car".to_string()));
}
