// esbuild.config.js
const path = require('path')

// This is a CommonJS module, so cannot be imported as an ES module
const ElmPlugin = require('esbuild-plugin-elm')
const esbuild = require('esbuild')

const node_env = process.env.RAILS_ENV
const watch = process.argv.includes("--watch");


const elm_options = function() {
    // can't see this log message?
    // console.log('node_env = ' + node_env);
    if (node_env === "production" || node_env === "arthur") {
        return {
            optimize: true,
            debug: false
        }
    } else {
        return {
            optimize: false,
            debug: true
        }
    }
}

const options = elm_options()

// watch option only works with esbuild < 0.16
// https://stackoverflow.com/questions/75221520/invalid-option-in-build-call-watch

const config = {
    entryPoints: ['./hello_elm.js'],
    bundle: true,
    outdir: path.join(process.cwd(), "app/assets/builds"),
    absWorkingDir: path.join(process.cwd(), "app/javascript"),
    watch: watch,
    sourcemap: true,
    plugins: [
        ElmPlugin(options) // options are documented below
    ],
}
esbuild.build(config);
// esbuild.context(config).then((r) => {
//     r.watch();
// });
