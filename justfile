repl:
    cargo run -p magus_repl

build-wasm:
    cargo build --target wasm32-unknown-unknown -p magus
