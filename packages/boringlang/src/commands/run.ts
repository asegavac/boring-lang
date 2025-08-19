import { defineCommand } from "@bunli/core";
import { boringGrammar } from "../parse/grammar";
import { semantics } from "../parse/semantics";

export const run = defineCommand({
  name: "run",
  description: "Run a boringlang file",

  handler: async ({ positional }) => {
    const [path] = positional;

    if (!path) {
      throw new Error("Usage: run <path>");
    }

    const file = Bun.file(path);
    const text = await file.text();

    const match = boringGrammar.match(text, "Module");
    if (match.succeeded()) {
      const adapter = semantics(match);
      const ast = adapter.toAST();
      console.log(JSON.stringify(ast, null, 2));
    } else {
      console.log(match.message);
      // console.log(boringGrammar.trace(text, "Module").toString());
    }
  },
});
