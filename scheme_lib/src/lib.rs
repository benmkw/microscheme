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
    clippy::unwrap_used,
    clippy::enum_glob_use
)]

use itertools::{EitherOrBoth::Both, Itertools};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[cfg(test)]
mod tests;

#[macro_export]
macro_rules! test_eq {
    ($lhs : expr => $rhs : expr) => {{
        assert_eq!($lhs, Some(Rc::new($rhs)))
    }};
}

#[macro_export]
macro_rules! list {
    ($( $elem: expr ),*) => {{
         List(vec![$( Rc::new($elem) ), *])
    }}
}

macro_rules! none_of {
    ($value: expr => $( $elem: expr ),*) => {{
         $( $value != $elem ) && *
    }}
}

macro_rules! any_of {
    ($value: expr => $( $elem: expr ),*) => {{
         $( $value == $elem ) || *
    }}
}

macro_rules! binary_op {
    ($exprs : expr, $env : expr => $operation : expr) => {{
        debug_assert!($exprs.len() == 3);

        eval(&[$exprs[1].clone()].clone(), &$env.clone()).and_then(|val1| {
            eval(&[$exprs[2].clone()].clone(), &$env.clone()).and_then(|val2| {
                if let (Number(n1), Number(n2)) = (&*val1, &*val2) {
                    Some(Rc::new(Number($operation(n1, n2))))
                } else {
                    unreachable!()
                }
            })
        })
    }};
}

macro_rules! cmp_op {
    ($exprs : expr, $env : expr => $op : expr) => {{
        debug_assert_eq!($exprs.len(), 3);
        eval(&[$exprs[1].clone()], &$env.clone()).and_then(|first_arg|{
            eval(&[$exprs[2].clone()], &$env.clone()).and_then(|second_arg|{
                if let (Expression::Number(n_one),Expression::Number(n_two)) = (&*first_arg,&*second_arg) {
                    if $op(n_one, n_two) {
                        Some(Rc::new(Expression::Number(121_212))) // or do ExprTrue which would be more accurate !!
                    } else {
                        Some(Rc::new(Expression::ExprFalse))
                    }
                } else if let (Expression::List(l_one),Expression::List(l_two)) = (&*first_arg,&*second_arg) {
                    let mut ret = None;
                    for tuple in l_one.iter().zip_longest(l_two.iter()) {
                        if let Both(one,two) = tuple {
                            let res = eval(&[Rc::new(List(vec![Rc::new(Expression::Equal), Rc::clone(one), Rc::clone(two)]))], $env);
                            if res == Some(Rc::new(Expression::ExprFalse)) {
                                ret = Some(Rc::new(Expression::ExprFalse));
                                break
                            }
                        } else {
                            ret = Some(Rc::new(Expression::ExprFalse));
                            break
                        }
                    }
                    ret.or(Some(Rc::new(Expression::Number(121_212)))) // or do ExprTrue which would be more accurate !!
                } else {
                    unreachable!()
                }
            })
        })
    }};
}

type REnvironment = Rc<RefCell<Environment>>;
pub type RExpression = Rc<Expression>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Number(i64),
    Symbol(String),
    SString(String), // TODO do interning
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    TokIf,
    TokFalse,
    Quote,
    ShortQuote,
    Define,
    Set,
    Lambda,
    Car,
    Cdr,
    Cons,
    LessThan,
    MoreThan,
    Equal,
    Let,
}

#[must_use]
pub fn tokenize(s: &str) -> Vec<Token> {
    use Token::*;

    let mut tokens = Vec::<Token>::new();

    let mut iter = s.chars().peekable();
    while let Some(c) = iter.peek() {
        if any_of!(*c => ' ', '\n', '\r', '\t') {
            iter.next();
            continue;
        }

        let token = match c {
            '+' => {
                iter.next();
                Plus
            }
            '-' => {
                iter.next();
                Minus
            }
            '*' => {
                iter.next();
                Star
            }
            '/' => {
                iter.next();
                Slash
            }
            '(' => {
                iter.next();
                LParen
            }
            ')' => {
                iter.next();
                RParen
            }
            '\'' => {
                iter.next();
                ShortQuote
            }
            '<' => {
                iter.next();
                LessThan
            }
            '>' => {
                iter.next();
                MoreThan
            }
            '=' => {
                iter.next();
                Equal
            }
            '\"' => {
                iter.next(); // consume first quote
                let string: String = iter.by_ref().take_while(|&c| c != '\"').collect();
                SString(string)
            }
            _ => {
                let ret: Token;

                if c.is_digit(10) {
                    let num: String = iter.take_while_ref(|&c| c.is_digit(10)).collect();

                    if let Ok(num) = num.parse::<i64>() {
                        ret = Number(num);
                    } else {
                        panic!("{:?} {}", num, "could not handle the number\n");
                    }
                } else {
                    let string: String = iter
                        .by_ref()
                        .take_while_ref(|&c| none_of!(c => ' ', '(', ')', '+', '-', '*', '/'))
                        .collect();
                    // TODO could/ should use a trie here for matching
                    match string.as_ref() {
                        "if" => ret = TokIf,
                        "#f" | "#F" => ret = TokFalse,
                        "quote" => ret = Quote,
                        "define" => ret = Define,
                        "set!" => ret = Set,
                        "lambda" => ret = Lambda,
                        "car" => ret = Car,
                        "cdr" => ret = Cdr,
                        "cons" => ret = Cons,
                        "let" => ret = Let,
                        _ => ret = Symbol(string),
                    };
                }
                ret
            }
        };
        tokens.push(token);
    }

    tokens
}

// TODO serialize to json https://github.com/serde-rs/json
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Number(i64),
    Symbol(String),
    SString(String), // TODO do interning
    List(Vec<RExpression>),
    Add,
    Sub,
    Mult,
    Div,
    ExprFalse,
    ExprIf,
    Quote,
    Define,
    Set,
    Lambda,
    Procedure(Rc<Proc>),
    Car,
    Cdr,
    Cons,
    LessThan,
    MoreThan,
    Equal,
}

fn inside_braces(token_iter: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>) -> Vec<Token> {
    let mut parenthesis_depth = 1;

    token_iter
        .take_while(|t| {
            match t {
                Token::LParen => parenthesis_depth += 1,
                Token::RParen => parenthesis_depth -= 1,
                _ => (),
            };
            parenthesis_depth != 0
        })
        .cloned()
        .collect()
}

#[must_use]
pub fn parse(tokens: &[Token]) -> Vec<RExpression> {
    let mut exprns = vec![];
    let mut token_iter = tokens.iter().peekable();

    while let Some(first_token) = token_iter.next() {
        use Expression::*;

        match first_token {
            Token::Number(x) => exprns.push(Rc::new(Number(*x))),
            Token::Symbol(s) => exprns.push(Rc::new(Symbol(s.clone()))),
            Token::SString(s) => exprns.push(Rc::new(SString(s.clone()))),

            Token::LParen => {
                if let Some(Token::Let) = token_iter.peek() {
                    let subexpr = inside_braces(&mut token_iter);
                    let mut new_expr = parse(&subexpr);

                    exprns.append(&mut new_expr);
                } else {
                    let subexpr = inside_braces(&mut token_iter);
                    let new_expr = parse(&subexpr);

                    exprns.push(Rc::new(List(new_expr)));
                }
            }

            Token::Plus => exprns.push(Rc::new(Add)),
            Token::Minus => exprns.push(Rc::new(Sub)),
            Token::Star => exprns.push(Rc::new(Mult)),
            Token::Slash => exprns.push(Rc::new(Div)),
            Token::TokIf => exprns.push(Rc::new(ExprIf)),
            Token::TokFalse => exprns.push(Rc::new(ExprFalse)),

            Token::LessThan => exprns.push(Rc::new(LessThan)),
            Token::MoreThan => exprns.push(Rc::new(MoreThan)),
            Token::Equal => exprns.push(Rc::new(Equal)),

            Token::Car => exprns.push(Rc::new(Car)),
            Token::Cdr => exprns.push(Rc::new(Cdr)),
            Token::Cons => exprns.push(Rc::new(Cons)),

            Token::Quote => exprns.push(Rc::new(Quote)),
            Token::ShortQuote => {
                // desugar to normal quote case
                if let Some(token) = token_iter.next() {
                    match token {
                        Token::LParen => {
                            let inside_values = List(parse(&inside_braces(&mut token_iter)));

                            let extended_inside_values =
                                vec![Rc::new(Quote), Rc::new(inside_values)];

                            exprns.push(Rc::new(List(extended_inside_values)));
                        }

                        Token::Symbol(s) => {
                            let inside_values = Symbol(s.to_string());

                            let extended_inside_values =
                                vec![Rc::new(Quote), Rc::new(inside_values)];

                            exprns.push(Rc::new(List(extended_inside_values)));
                        }

                        Token::Number(x) => {
                            let inside_values = Number(*x);

                            let extended_inside_values =
                                vec![Rc::new(Quote), Rc::new(inside_values)];

                            exprns.push(Rc::new(List(extended_inside_values)));
                        }
                        _ => unreachable!(),
                    }
                } else {
                    unreachable!()
                }
            }

            // desugar this to define and expr
            Token::Let => {
                let next = token_iter.next();
                debug_assert_eq!(next, Some(&Token::LParen)); // begin of defines

                let mut define_exprs = vec![];
                let defines = inside_braces(&mut token_iter);
                let mut defines = defines.iter();

                while let Some(&Token::LParen) = defines.next() {
                    let next_key = defines.next().unwrap();
                    let next_value = defines.next().unwrap();

                    let mut curr_define = vec![Rc::new(Define)];
                    curr_define.append(&mut parse(&[next_key.clone()]));
                    curr_define.append(&mut parse(&[next_value.clone()]));
                    define_exprs.push(Rc::new(List(curr_define)));
                    let next = defines.next();
                    debug_assert_eq!(next, Some(&Token::RParen)); // end of curr define
                }

                // begin of bodies
                let body = inside_braces(&mut token_iter);

                let mut body = parse(&body);

                exprns.append(&mut define_exprs);
                exprns.append(&mut body);
            }

            Token::Define => exprns.push(Rc::new(Define)),
            Token::Set => exprns.push(Rc::new(Set)),
            Token::Lambda => exprns.push(Rc::new(Lambda)),

            Token::RParen => unreachable!(),
        }
    }

    exprns
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Environment {
    symbolmap: HashMap<String, RExpression>,
    outer: Option<REnvironment>,
}

impl Environment {
    fn get(&self, s: &str) -> RExpression {
        match self.symbolmap.get(s) {
            Some(e) => Rc::clone(e),
            None => self.outer.as_ref().unwrap().borrow().get(s),
        }
    }

    fn set(&mut self, k: &[RExpression], v: &[RExpression]) {
        for (k, v) in k.iter().zip(v.iter()) {
            // actually does insert or update which is the same as pythons update method on dicts
            if let Expression::Symbol(s) = &**k {
                self.symbolmap.insert(s.to_string(), Rc::clone(v));
            } else {
                unreachable!()
            }
        }
    }

    fn new_with(params: &[RExpression], args: &[RExpression], outer: REnvironment) -> REnvironment {
        let env = Rc::new(RefCell::new(Self::default()));

        // maybe this can be more streamlined
        env.borrow_mut().set(params, args);
        env.borrow_mut().outer = Some(outer);

        env
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Proc {
    params: RExpression,
    body: RExpression,
    // a Proc captures the variables from its surrounding scope (should this be done by reference?)
    // in normal Scheme this is done by refernce, these values can be inlined in this implementation as they have value semantics
    // maybe because of the callSTACK doing this by reference is not necessary
    env: REnvironment,
}

impl Proc {
    fn new(params: RExpression, body: RExpression, env: REnvironment) -> Self {
        if let Expression::List(_) = *params {
        } else {
            unreachable!()
        }

        Self { params, body, env }
    }

    // ((define twice (lambda (x) (* 2 x))) (twice 5))
    // set the symbols of own params to the values of the args given (e.g. set x to 5)
    fn call(&self, args: &[RExpression]) -> Option<RExpression> {
        if let Expression::List(params) = &*self.params {
            // dbg!(&self.env); // TODO seems to be recursive somehow???
            eval(
                &[Rc::clone(&self.body)],
                &Environment::new_with(params, args, Rc::clone(&self.env)),
            )
        } else {
            unreachable!();
        }
    }
}

pub fn eval(expressions: &[RExpression], env: &REnvironment) -> Option<RExpression> {
    use Expression::*;

    let mut last_res = None;

    for expression in expressions {
        let new_res = match &**expression {
            ExprFalse => Some(Rc::new(ExprFalse)),
            Number(x) => Some(Rc::new(Number(*x))),
            SString(x) => Some(Rc::new(SString(x.to_string()))),

            Procedure(procedure) => Some(Rc::new(Procedure(Rc::clone(procedure)))),

            Add => Some(Rc::new(Add)),
            Sub => Some(Rc::new(Sub)),
            Div => Some(Rc::new(Div)),
            Mult => Some(Rc::new(Mult)),

            LessThan => Some(Rc::new(LessThan)),
            MoreThan => Some(Rc::new(MoreThan)),
            Equal => Some(Rc::new(Equal)),

            Car => Some(Rc::new(Car)),
            Cdr => Some(Rc::new(Cdr)),
            Cons => Some(Rc::new(Cons)),

            Symbol(s) => Some(env.borrow().get(s)),

            List(exprs) => match &*exprs[0] {
                Number(x) => {
                    // special, maybe remove at some point?
                    debug_assert_eq!(exprs.len(), 1);
                    Some(Rc::new(Number(*x)))
                }
                ExprFalse => {
                    // special, maybe remove at some point?
                    debug_assert_eq!(exprs.len(), 1);
                    Some(Rc::new(ExprFalse))
                }

                // short circuit, no need to go though environment lookup
                Add => binary_op!(exprs, env => |a, b| a + b),
                Sub => binary_op!(exprs, env => |a, b| a - b),
                Mult => binary_op!(exprs, env => |a, b| a * b),
                Div => binary_op!(exprs, env => |a, b| a / b),

                LessThan => cmp_op!(exprs, env => |a, b| a < b),
                MoreThan => cmp_op!(exprs, env => |a, b| a > b),
                Equal => cmp_op!(exprs, env => |a, b| a == b),

                Car => {
                    debug_assert_eq!(exprs.len(), 2);
                    eval(&[Rc::clone(&exprs[1])], env).and_then(|car| {
                        if let List(expr) = &*car {
                            Some(Rc::clone(&expr[0]))
                        } else {
                            unreachable!()
                        }
                    })
                }
                Cdr => {
                    debug_assert_eq!(exprs.len(), 2);
                    eval(&[Rc::clone(&exprs[1])], env).and_then(|car| {
                        if let List(expr) = &*car {
                            Some(Rc::new(List(expr[1..].to_vec())))
                        } else {
                            unreachable!()
                        }
                    })
                }
                Cons => {
                    debug_assert_eq!(exprs.len(), 3);
                    eval(&[Rc::clone(&exprs[1])], env).and_then(|exprs_one| {
                        eval(&[Rc::clone(&exprs[2])], env).map(|exprs_two| {
                            // do not concatenate the empty list onto sth.
                            if *exprs_two == List(vec![]) {
                                Rc::new(List(vec![exprs_one]))
                            } else {
                                Rc::new(List(vec![exprs_one, exprs_two]))
                            }
                        })
                    })
                }

                Quote => {
                    debug_assert_eq!(exprs.len(), 2);
                    Some(Rc::clone(&exprs[1]))
                }

                Define => {
                    debug_assert_eq!(exprs.len(), 3);
                    let symbol = Rc::clone(&exprs[1]);
                    let ret = eval(&[Rc::clone(&exprs[2])], env).unwrap();
                    env.borrow_mut().set(&[symbol], &[ret]);
                    None
                }

                Set => {
                    debug_assert_eq!(exprs.len(), 3);
                    if let Some(res) = eval(&[Rc::clone(&exprs[2])], env) {
                        env.borrow_mut().set(&[Rc::clone(&exprs[1])], &[res]);
                    } else {
                        unreachable!()
                    }
                    None
                }

                Lambda => {
                    debug_assert_eq!(exprs.len(), 3);
                    Some(Rc::new(Procedure(Rc::new(Proc::new(
                        Rc::clone(&exprs[1]),
                        Rc::clone(&exprs[2]),
                        Rc::clone(env),
                    )))))
                }

                ExprIf => {
                    debug_assert_eq!(exprs.len(), 4);
                    eval(&[Rc::clone(&exprs[1])], env).and_then(|val| {
                        if val == Rc::new(ExprFalse) {
                            eval(&[Rc::clone(&exprs[3])], env) // ExprFalse is only false value
                        } else {
                            eval(&[Rc::clone(&exprs[2])], env) // .. everything else than ExprFalse, is true
                        }
                    })
                }

                // should never happen inside the list,
                Procedure(_) | SString(_) => {
                    panic!("{:?}", &exprs);
                }

                // procedure call
                // List would be a lambda which would first get evaluated
                // symbol would be the name of a function and thus get resolved to one
                Symbol(_) | List(_) => {
                    let firstval = eval(&[Rc::clone(&exprs[0])], env).unwrap();

                    if let Procedure(procedure) = &*firstval {
                        let args = exprs[1..]
                            .iter()
                            .filter_map(|e| eval(&[Rc::clone(e)], env))
                            .collect::<Vec<RExpression>>();
                        procedure.call(&args)
                    } else {
                        // when redefining builtins
                        let mut expr = vec![firstval];
                        expr.extend(exprs[1..].iter().cloned());
                        // situation is like like exprs = List([Add Number(3) Number(2)])
                        eval(&[Rc::new(List(expr))], env)
                    }
                }
            },

            // these should only ever happen inside the list
            ExprIf | Quote | Define | Set | Lambda => {
                panic!("{:?}", &expression);
            }
        };

        if new_res.is_some() {
            last_res = new_res;
        }
    }

    last_res
}

#[must_use]
pub fn run(input: &str) -> Option<RExpression> {
    // println!("{:?}", input);

    let tokens: Vec<Token> = tokenize(input);
    // dbg!(&tokens);

    let expression = parse(&tokens);
    // dbg!(&expression);

    let environment = Rc::new(RefCell::new(Environment::default()));
    eval(&expression, &environment)

    // print!("DONE \n\n\n");
}
