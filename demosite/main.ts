import { MagusFuel, MagusInterpreter } from "./pkg/magus_web.js";

const interp = new MagusInterpreter(["demosite"]);
interp.enable_base();
interp.enable_cxr();
const thread = interp.new_thread();

const fuel = new MagusFuel(1_000);
const chunk = thread.compile(
  "web-input.scm",
  "(import (scheme base)) (values (features) (cons '() 1) (string #\\x1face) #0=#(1 #0# 3 ))",
  false,
  10_000,
);

console.log(chunk.instructions());

thread.load(chunk);
while (thread.run(fuel)) {
  fuel.refill(1000, 1000);
}
console.log(thread.result(true));
console.log(interp.symbol_dump());

// const p = document.createElement("p");
// p.textContent = thread.result().toString();
// document.body.appendChild(p);
