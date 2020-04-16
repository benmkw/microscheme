use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use scheme_lib::*;

// TODO maybe call src/test.py and compare other functions, overhead of starting shell would be unfortunate though
fn criterion_benchmark(c: &mut Criterion) {
    let input = "(define a '(1 2 3)) (cdr a)";
    c.bench_function("cdr", |b| b.iter(|| run(black_box(input))));
}

fn throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("throughput");

    let base_input = "(define a '(1 2 3)) (cdr a)";
    let strlen = base_input.chars().count();

    for size in [1000].iter() {
        let input: String = std::iter::repeat(base_input).take(*size).collect();
        // let input = format!("{}{}", input, base_input);

        group.throughput(Throughput::Bytes((*size * strlen) as u64));

        group.bench_with_input(BenchmarkId::new("scheme", *size), &input, |b, i| {
            b.iter(|| run(black_box(&i)))
        });
    }

    group.finish();
}

criterion_group!(benches, throughput);
// criterion_group!(benches, criterion_benchmark, throughput);
criterion_main!(benches);
