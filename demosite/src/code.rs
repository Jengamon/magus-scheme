use crate::dialog::{Dialog, show_modal};
use codee::string::{FromToStringCodec, JsonSerdeCodec};
use leptos::{
    ev::{KeyboardEvent, MouseEvent},
    html::{Div, Textarea},
    prelude::*,
};
use leptos_use::{
    UseScrollReturn,
    storage::{UseStorageOptions, use_local_storage, use_local_storage_with_options},
    use_scroll,
};
use phosphor_leptos::{GEAR_FINE, Icon, IconWeight, LIST_NUMBERS};
use serde::{Deserialize, Serialize};
use syntect::{highlighting::ThemeSet, html::highlighted_html_for_string, parsing::SyntaxSet};

use web_sys::HtmlTextAreaElement;

// Perceived color threshold where icons go bright to dark
// https://en.wikipedia.org/wiki/Lightness
// L* is on the same scale as Y which is 0-100 apparently
const PB_THRESHOLD: f32 = 50.0;

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
struct CodeEditorSettings {
    tab_size: usize,
    show_line_numbers: bool,
}

impl Default for CodeEditorSettings {
    fn default() -> Self {
        Self {
            tab_size: 2,
            show_line_numbers: true,
        }
    }
}

// Based off code from https://css-tricks.com/creating-an-editable-textarea-that-supports-syntax-highlighted-code/
#[component]
pub fn CodeEditor(
    #[prop(into)] code: Signal<String>,
    set_code: WriteSignal<String>,
) -> impl IntoView {
    let ps = StoredValue::new(SyntaxSet::load_defaults_newlines());
    let ts = StoredValue::new(ThemeSet::load_defaults());
    // TODO Allow theme selection
    let (thema, set_thema, remove_thema) =
        use_local_storage_with_options::<String, FromToStringCodec>(
            "code-editor.theme",
            UseStorageOptions::default().initial_value("base16-ocean.dark"),
        );
    let (settings, set_settings, _) =
        use_local_storage::<CodeEditorSettings, JsonSerdeCodec>("code-editor.settings");

    if ts.with_value(|ts| thema.with_untracked(|thema| !ts.themes.contains_key(thema))) {
        remove_thema()
    }

    let theme = move || ts.with_value(|ts| thema.with(|thema| ts.themes[thema].clone()));
    let theme_bg = move || {
        theme()
            .settings
            .background
            .map(|c| format!("#{:02x}{:02x}{:02x}{:02x}", c.r, c.g, c.b, c.a))
    };
    let theme_bg_brightness = move || {
        theme()
            .settings
            .background
            .map(|c| {
                // Stolen from https://stackoverflow.com/a/56678483
                #[inline(always)]
                /// Send this function a decimal sRGB gamma encoded color value
                /// between 0.0 and 1.0, and it returns a linearized value.
                fn srgb_to_lin(cc: f32) -> f32 {
                    if cc <= 0.04045 {
                        cc / 12.92
                    } else {
                        ((cc + 0.055) / 1.055).powf(2.4)
                    }
                }
                #[inline(always)]
                /// Send this function a luminance value between 0.0 and 1.0,
                /// and it returns L* which is "perceptual lightness"
                fn y_to_lstar(y: f32) -> f32 {
                    if y <= 216. / 24389. {
                        y * 24389. / 27.
                    } else {
                        y.powf(1. / 3.) * 116. - 16.
                    }
                }
                let vr = c.r as f32 / 255.0;
                let vg = c.g as f32 / 255.0;
                let vb = c.b as f32 / 255.0;
                let y =
                    0.2126 * srgb_to_lin(vr) + 0.7152 * srgb_to_lin(vg) + 0.0722 * srgb_to_lin(vb);
                y_to_lstar(y)
            })
            .unwrap_or(0.)
    };

    let highlights = NodeRef::<Div>::new();
    let lines = NodeRef::<Div>::new();
    let editing = NodeRef::<Textarea>::new();
    let syntax = ps
        .get_value()
        .find_syntax_by_extension("scm")
        .unwrap()
        .clone();

    let UseScrollReturn {
        x: editor_x,
        y: editor_y,
        ..
    } = use_scroll(editing);
    let UseScrollReturn {
        set_x: set_highlights_x,
        set_y: set_highlights_y,
        ..
    } = use_scroll(highlights);
    let UseScrollReturn {
        set_y: set_lines_y,
        set_x: set_lines_x,
        ..
    } = use_scroll(lines);

    // Keep the scrolling synced together
    Effect::new(move |_| {
        set_highlights_x(editor_x.get());
        set_highlights_y(editor_y.get());
        set_lines_x(editor_x.get());
        set_lines_y(editor_y.get());
    });

    let lines_total = move || code.with(|code| code.split('\n').count());
    let keydown_handler = move |ev: KeyboardEvent| {
        if let "Tab" = ev.key().as_str() {
            ev.prevent_default();
            let target: HtmlTextAreaElement = event_target(&ev);
            let start = target.selection_start().unwrap().unwrap() as usize;
            let end = target.selection_end().unwrap().unwrap() as usize;
            let code = code.get();
            set_code.set([&code[0..start], "\t", &code[end..]].into_iter().collect());
            target.set_selection_start(Some(start as u32 + 1)).unwrap();
            target.set_selection_end(Some(start as u32 + 1)).unwrap();
        }
    };

    let highlighted_html = move || {
        code.with(|code| {
            let mut code = code.clone();
            if code.ends_with('\n') {
                code += " ";
            }
            highlighted_html_for_string(&code, &ps.get_value(), &syntax, &theme())
        })
        .unwrap()
    };

    let settings_dialog = move || {
        let theme_listing = move || {
            ts.with_value(|ts| {
                ts.themes
                    .keys()
                    .map(|key| {
                        let is_theme = thema.with(|thema| thema == key);
                        view! { <option selected=is_theme>{key.to_string()}</option> }
                    })
                    .collect_view()
            })
        };

        view! {
            <Dialog id="codeeditor_settings_modal">
                <fieldset class="fieldset w-sm m-auto text-md bg-base-200 rounded-box p-4 grid-cols-2">
                    <legend class="fieldset-legend">Settings</legend>
                    <label class="fieldset-label">"Theme"</label>
                    <select
                        id="code-editor__theme-select"
                        class="select select-bordered"
                        on:change=move |ev| set_thema.set(event_target_value(&ev))
                    >
                        {theme_listing}
                    </select>

                    <label class="fieldset-label">"Tab Size"</label>
                    <select
                        id="code-editor__tab-size-select"
                        class="select select-bordered"
                        on:change=move |ev| {
                            set_settings
                                .update(|settings| {
                                    settings.tab_size = event_target_value(&ev).parse().unwrap();
                                })
                        }
                    >

                        <option selected=move || settings.with(|s| s.tab_size == 2)>"2"</option>
                        <option selected=move || settings.with(|s| s.tab_size == 4)>"4"</option>
                        <option selected=move || settings.with(|s| s.tab_size == 8)>"8"</option>
                    </select>
                </fieldset>
            </Dialog>
        }
    };

    view! {
        {settings_dialog()}
        <div class="relative min-h-24 grow p-0 m-0">
            <div class="flex flex-row-reverse absolute top-0 right-0 w-full">
                <MenuIcon
                    theme_bg_brightness=Signal::derive(theme_bg_brightness)
                    on_click=move |_ev| { show_modal("codeeditor_settings_modal") }
                >

                    <Icon icon=GEAR_FINE size="24px" />
                </MenuIcon>
                <ToggleMenuIcon
                    toggle=move || {
                        set_settings
                            .update(|settings| {
                                settings.show_line_numbers = !settings.show_line_numbers;
                            })
                    }

                    is_active=Signal::derive(move || { settings.read().show_line_numbers })
                    // theme_bg_brightness=theme_bg_brightness
                    theme_bg_brightness=Signal::derive(move || { 0.0 })
                >
                    <Icon icon=LIST_NUMBERS size="24px" weight=IconWeight::Light />
                </ToggleMenuIcon>
            </div>
            <textarea
                node_ref=editing
                id="code-editor__editor"
                class="textarea font-mono resize-none overflow-auto whitespace-pre
                absolute px-2 py-1 m-0 top-0 left-0 z-40 w-full h-full
                text-transparent bg-transparent rounded-none leading-snug text-sm"
                spellcheck="false"
                class=("!pl-12", move || settings.read().show_line_numbers)
                style:tab-size=Signal::derive(move || settings.read().tab_size.to_string())
                class=("caret-white", move || { theme_bg_brightness() < PB_THRESHOLD })
                class=("caret-black", move || { theme_bg_brightness() > PB_THRESHOLD })
                bind:value=(code, set_code)
                on:keydown=keydown_handler
            >

                {
                    let mut text = code.get_untracked();
                    if text.ends_with('\n') {
                        text += " ";
                    }
                    text
                }

            </textarea>
            <div
                class="absolute m-0 p-0 top-0 left-0 z-0 w-full h-full
                overflow-hidden flex flex-row items-stretch"
                style=("background-color", move || theme_bg().unwrap_or("inherit".to_string()))
            >
                <Show when=move || settings.read().show_line_numbers>
                    <div
                        node_ref=lines
                        class="font-mono w-10 bg-base-200 px-2 py-1 m-0 text-right
                        overflow-y-auto no-scrollbar"
                    >
                        {move || {
                            (0..lines_total())
                                .map(|_| {
                                    view! {
                                        <span
                                            style=("counter-increment", "linenumber")
                                            class="block leading-snug text-sm before:content-[counter(linenumber)]"
                                        ></span>
                                    }
                                })
                                .collect_view()
                        }}

                    </div>
                </Show>
                <div
                    node_ref=highlights
                    aria-hidden="true"
                    style:tab-size=Signal::derive(move || settings.read().tab_size.to_string())
                    class="textarea font-mono overflow-hidden whitespace-pre px-2 py-1
                    w-full cursor-text rounded-none bg-inherit grow leading-snug text-sm"
                    inner_html=highlighted_html
                />
            </div>
        </div>
    }
}

#[component]
fn ToggleMenuIcon<F: Fn() + 'static>(
    #[prop(into)] theme_bg_brightness: Signal<f32>,
    #[prop(into)] is_active: Signal<bool>,
    toggle: F,
    children: Children,
) -> impl IntoView {
    view! {
        <button
            type="button"
            class="z-[50]"
            class=("!text-primary", is_active)

            class=("text-white/50", move || { theme_bg_brightness.with(|tbb| *tbb < PB_THRESHOLD) })
            class=("text-black/50", move || { theme_bg_brightness.with(|tbb| *tbb > PB_THRESHOLD) })
            class=(
                "hover:text-white",
                move || { theme_bg_brightness.with(|tbb| *tbb < PB_THRESHOLD) },
            )

            class=(
                "hover:text-black",
                move || { theme_bg_brightness.with(|tbb| *tbb > PB_THRESHOLD) },
            )

            on:click=move |_| toggle()
        >

            {children()}
        </button>
    }
}

#[component]
fn MenuIcon<F: Fn(MouseEvent) + 'static>(
    #[prop(into)] theme_bg_brightness: Signal<f32>,
    on_click: F,
    children: Children,
) -> impl IntoView {
    view! {
        <button
            type="button"
            class="z-[50]"
            class=("text-white/50", move || { theme_bg_brightness.with(|tbb| *tbb < PB_THRESHOLD) })
            class=("text-black/50", move || { theme_bg_brightness.with(|tbb| *tbb > PB_THRESHOLD) })
            class=(
                "hover:text-white",
                move || { theme_bg_brightness.with(|tbb| *tbb < PB_THRESHOLD) },
            )

            class=(
                "hover:text-black",
                move || { theme_bg_brightness.with(|tbb| *tbb > PB_THRESHOLD) },
            )

            on:click=on_click
        >

            {children()}
        </button>
    }
}
