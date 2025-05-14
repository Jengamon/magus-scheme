#![allow(non_snake_case)]
mod code;
mod dialog;
mod root;

use leptos::prelude::*;

fn main() {
    console_error_panic_hook::set_once();
    let _ = console_log::init();

    mount_to_body(|| view! {<root::App />});
}
