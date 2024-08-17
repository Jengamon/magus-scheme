repl:
    cargo run -p magus_repl

test *FLAGS:
    cargo nextest r -E "not test(arbtest)" {{FLAGS}}

arbtest *FLAGS:
    cargo nextest r -E "test(arbtest)" {{FLAGS}}

build-wasm:
    cargo build --target wasm32-unknown-unknown -p magus
