//! App-wide utils for showing dialogs/modals
use leptos::prelude::*;

/// Request model with given `id` be shown
pub fn show_modal(id: impl AsRef<str>) {
    use wasm_bindgen::JsCast;
    use web_sys::js_sys::{Function, Reflect};

    let modal = document().get_element_by_id(id.as_ref());
    if let Some(modal) = modal {
        let showModal_func =
            Reflect::get(&modal, &"showModal".into()).and_then(|func| func.dyn_into::<Function>());
        if let Ok(showModal_func) = showModal_func {
            showModal_func.call0(&modal).unwrap();
        }
    }
}

/// Request model with given `id` be closed
pub fn close_modal(id: impl AsRef<str>) {
    use wasm_bindgen::JsCast;
    use web_sys::js_sys::{Function, Reflect};

    let modal = document().get_element_by_id(id.as_ref());
    if let Some(modal) = modal {
        let showModal_func =
            Reflect::get(&modal, &"close".into()).and_then(|func| func.dyn_into::<Function>());
        if let Ok(showModal_func) = showModal_func {
            showModal_func.call0(&modal).unwrap();
        }
    }
}

/// Default dialog
#[component]
pub fn Dialog(#[prop(into)] id: String, children: Children) -> impl IntoView {
    view! {
        <dialog id=id.clone() class="modal">
            <div class="modal-box overflow-auto">{children()}</div>
            <form method="dialog" class="modal-backdrop">
                <button>"close"</button>
            </form>
        </dialog>
    }
}
