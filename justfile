repl:
    cargo run -p magus_repl

test:
    cargo test

build-wasm:
    cargo build --target wasm32-unknown-unknown -p magus
