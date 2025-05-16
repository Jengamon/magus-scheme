import { StreamLanguage } from "@codemirror/language";
import { scheme } from "@codemirror/legacy-modes/mode/scheme";

import { basicSetup } from "codemirror";
import { EditorState } from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import { indentWithTab } from "@codemirror/commands";
import { birdsOfParadise } from "thememirror";

import { createEffect, createSignal, onMount } from "solid-js";
import { makePersisted } from "@solid-primitives/storage";

import { z } from "zod";

import { MagusFuel, MagusInterpreter, MagusThread } from "../pkg/magus_web.js";

import "@phosphor-icons/web/fill";

function ScriptExec() {
  let editor;
  // TODO Make this a prop
  const initial_doc = `(import (scheme base))
(define x 20)
(define y 30)
(+ x y)`;
  const [doc, setDoc] = makePersisted(createSignal(initial_doc), {
    storage: sessionStorage,
    name: "magus:editor-document",
  });
  onMount(() => {
    const solidDocUpdate = EditorView.updateListener.of((update) => {
      if (update.docChanged) {
        setDoc(update.state.doc.toString());
      }
    });
    const state = EditorState.create({
      doc: doc(),
      extensions: [
        basicSetup,
        keymap.of([indentWithTab]),
        StreamLanguage.define(scheme),
        birdsOfParadise,
        solidDocUpdate,
      ],
    });
    const _view = new EditorView({
      state,
      parent: editor,
    });
  });
  const interpreter = new MagusInterpreter(["demosite"]);
  interpreter.enable_base();
  interpreter.enable_cxr();
  interpreter.enable_inexact();
  interpreter.enable_lazy();

  let thread: MagusThread | null = null;

  const [output, setOutput] = createSignal("");
  const [error, setError] = createSignal(null as string | null);
  const [instDump, setInstDump] = createSignal([] as string[]);
  const [caseInsensitive, setCaseInsensitive] = createSignal(false);

  const errSchema = z.string();
  function executeScript() {
    const source = doc();

    thread = interpreter.new_thread();
    setOutput("Loading...");
    setError(null);
    try {
      const chunk = thread.compile(
        "web-input.scm",
        source,
        caseInsensitive(),
        10_000,
      );
      setInstDump(chunk.instructions());

      thread.load(chunk);
    } catch (e) {
      thread = null;
      setError(errSchema.parse(e));
      setOutput("");
    }
  }

  const fuel = new MagusFuel(1_000);
  setInterval(() => {
    if (thread && !thread.is_finished()) {
      fuel.refill(1_000, 1_000);
      thread.run(fuel);
    } else if (thread) {
      try {
        setOutput(JSON.stringify(thread.result()));
        setError(null);
      } catch (e) {
        setError(errSchema.parse(e));
        setOutput("");
      }
      thread = null;
    }
  }, 100);

  createEffect(() => {
    if (instDump().length > 0) {
      console.log(instDump());
    }
  });

  return (
    <>
      <p>Press Esc, then (Shift+)Tab to switch focus from editor</p>
      <label class="label">
        <input
          type="checkbox"
          class="checkbox"
          onChange={() => setCaseInsensitive((ci) => !ci)}
        />
        Case Insensitive
      </label>
      <div ref={editor}></div>
      <button class="btn" type="button" onClick={executeScript}>
        <i class="ph-fill ph-play" />Run
      </button>
      <p class="font-mono whitespace-pre">{output()}</p>
      <p class="text-error font-mono whitespace-pre">{error()}</p>
    </>
  );
}

export default ScriptExec;
