repl *FLAGS:
    cargo run -p magus_repl -- {{FLAGS}}

clear-locale-data:
    rm -Rf locale_data

locale-data: clear-locale-data
    icu4x-datagen --keys none --locales und --format mod --out locale_data

test *FLAGS:
    cargo nextest r -E "not test(arbtest)" {{FLAGS}}

test-ci:
    cargo nextest -Pci r -E "not test(arbtest)"
    cargo nextest -Pci r -E "not test(arbtest)" -p datatest_parse

lexer-test *FLAGS:
    cargo nextest r -E "test(lxd)" {{FLAGS}}

parser-test *FLAGS:
    cargo nextest r -E "test(gpd)" {{FLAGS}}

scheme-test *FLAGS:
    cargo nextest r -E "test(sct)" {{FLAGS}}

arbtest *FLAGS:
    cargo nextest r -E "test(arbtest)" {{FLAGS}}

arbtest-ci:
    cargo nextest -Pci r -E "test(arbtest)"

# Meant for CI
build-wasm: locale-data
    ICU4X_DATA_DIR=$(pwd)/locale_data cargo build --target wasm32-unknown-unknown -p magus

doc:
    cargo doc --document-private-items --no-deps --open
