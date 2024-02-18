#! /usr/bin/env node

import { parseArgs } from "node:util";
import esbuild from "esbuild";

const { values: args } = parseArgs({
    options: {
        watch: {
            type: "boolean",
            multiple: false,
        },
        serve: {
            type: "boolean",
            multiple: false,
        },
        main: {
            type: "string",
            multiple: false,
        },
    },
    args: process.argv.slice(2),
});

const ctx = await esbuild.context({
    stdin: {
        contents: `import { main } from "./output/${args.main}/index.js";main();`,
        resolveDir: '.'
    },
    bundle: true,
    platform: 'browser',
    outfile: 'docs/app.js',
    minify: true,
});

await ctx.rebuild();

if (args.serve) {
    await ctx.serve({
        servedir: 'docs',
    });
}

if (args.watch) {
    await ctx.watch();
}

if (!args.watch && !args.serve) {
    await ctx.dispose();
}
