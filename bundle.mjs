#! /usr/bin/env node

import { parseArgs } from "node:util";
import esbuild from "esbuild";

const { values: args } = parseArgs({
    options: {
        serve: {
            type: "boolean",
            multiple: false,
        },
    },
    args: process.argv.slice(2),
});

const ctx = await esbuild.context({
    entryPoints: [`app/app.js`],
    bundle: true,
    platform: 'browser',
    outdir: 'docs',
    minify: true,
    loader: {
        ".woff2": "file",
    },
});

await ctx.rebuild();

if (args.serve) {
    await ctx.serve({
        servedir: 'docs',
    });
    await ctx.watch();
} else {
    await ctx.dispose();
}
