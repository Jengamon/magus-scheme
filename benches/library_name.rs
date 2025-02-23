use divan::Bencher;
use magus::compiler::LibraryNameItem;
use std::rc::Rc;
use std::sync::Arc;

fn main() {
    divan::main();
}

type ArcLibraryName = Arc<[LibraryNameItem]>;
type RcLibraryName = Rc<[LibraryNameItem]>;
type BaselineLibraryName = Box<[LibraryNameItem]>;

fn clone_and_drop_library_names<T: FromIterator<LibraryNameItem> + Clone>(bencher: Bencher) {
    let mut interner = lasso::Rodeo::new();
    let scheme = interner.get_or_intern_static("scheme");
    let base = interner.get_or_intern_static("base");
    let srfi = interner.get_or_intern_static("srfi");
    // This is not *exactly* the library name as used, but lets check it out
    let lib_name = T::from_iter([
        LibraryNameItem::Identifier(scheme),
        LibraryNameItem::Identifier(base),
        LibraryNameItem::Identifier(srfi),
        LibraryNameItem::Integer(1),
    ]);

    bencher.bench_local(move || {
        for _ in 0..10 {
            let _li = lib_name.clone();
        }
    });
}

#[divan::bench]
fn arc_bench(bencher: Bencher) {
    clone_and_drop_library_names::<ArcLibraryName>(bencher);
}

#[divan::bench]
fn rc_bench(bencher: Bencher) {
    clone_and_drop_library_names::<RcLibraryName>(bencher);
}

#[divan::bench]
fn baseline_bench(bencher: Bencher) {
    clone_and_drop_library_names::<BaselineLibraryName>(bencher);
}
