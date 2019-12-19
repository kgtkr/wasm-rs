# wasm-rs

## Run Examples

```
$ cargo build --all-targets --release

$ cargo run --example cl8w-ex --release

$ cargo run --example cl8w-gcd --release

$ (cd md5-bin && cargo build --target wasm32-unknown-unknown --release)
$ cargo run --example md5 --release

$ (cd cl8w-on-wasm-bin && cargo build --target wasm32-unknown-unknown --release)
$ cargo run --example cl8w-on-wasm-on-wasm --release
```

## Test

```
$ git submodule init
$ git submodule update
$ ./build-spec.sh
$ cargo test
```