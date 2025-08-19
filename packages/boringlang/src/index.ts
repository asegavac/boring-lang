#!/usr/bin/env bun
import { createCLI } from "@bunli/core";

import { run } from "./commands/run.js";

const cli = createCLI({
  name: "boringlang",
  version: "0.1.0",
  description: "Boring programming language CLI",
});

cli.command(run);

await cli.run();
