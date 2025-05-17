import { defineConfig } from "vite";
import solidPlugin from "vite-plugin-solid";
import tailwindPlugin from "@tailwindcss/vite";

export default defineConfig({
  plugins: [solidPlugin(), tailwindPlugin()],
  optimizeDeps: {
    esbuildOptions: {
      target: "esnext",
    },
  },
  build: {
    target: "esnext",
  },
});
