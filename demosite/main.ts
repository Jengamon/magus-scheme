import { MagusFuel, MagusInterpreter } from "./pkg/magus_web.js";

const interp = new MagusInterpreter(["demosite"]);
interp.enable_base();
const thread = interp.new_thread();

const fuel = new MagusFuel(1_000);
const chunk = thread.compile(
  "web-input.scm",
  "(import (scheme base)) (values (features) )",
  false,
  10_000,
);

thread.load(chunk);
while (thread.run(fuel)) {
  fuel.refill(1000, 1000);
}
console.log(thread.result());

const p = document.createElement("p");
p.textContent = JSON.stringify(thread.result());
document.body.appendChild(p);
