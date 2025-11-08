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
    // entryPoints をオブジェクトで指定して出力先フォルダを固定する
    entryPoints: {
        "js/app": "app/app.js",
        "css/app": "app/app.css",
    },
    bundle: true,
    platform: 'browser',
    outdir: 'docs',
    minify: true,
    loader: {
        ".woff2": "file",
    },
    assetNames: 'fonts/[name]',
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
