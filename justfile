repl:
    cargo run -p magus_repl

clear-locale-data:
    rm -Rf locale_data

locale-data: clear-locale-data
    icu4x-datagen --keys none --locales und --format mod --out locale_data

test *FLAGS:
    cargo nextest r -E "not test(arbtest)" {{FLAGS}}

arbtest *FLAGS:
    cargo nextest r -E "test(arbtest)" {{FLAGS}}

# Meant for CI
build-wasm: locale-data
    ICU4X_DATA_DIR=$(pwd)/locale_data cargo build --target wasm32-unknown-unknown -p magus
