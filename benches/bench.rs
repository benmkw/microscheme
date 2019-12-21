use criterion::{black_box, criterion_group, criterion_main, Criterion};
use scheme_lib::*;

// TODO maybe call src/test.py and compare other functions, overhead of starting shell would be unfortunate though
fn criterion_benchmark(c: &mut Criterion) {
    let input = "(define a '(1 2 3)) (cdr a)";
    c.bench_function("cdr", |b| b.iter(|| run(black_box(input))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
