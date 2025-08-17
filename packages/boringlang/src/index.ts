#!/usr/bin/env bun
import { createCLI } from "@bunli/core";

const cli = createCLI({
  name: "boringlang",
  version: "0.1.0",
  description: "Boring programming language CLI",
});


await cli.run();
