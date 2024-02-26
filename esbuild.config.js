// esbuild.config.js
const path = require('path')
const ElmPlugin = require('esbuild-plugin-elm')
const esbuild = require('esbuild')

const elm_options = function() {
    // if (process.env.NODE_ENV === "production") {
        return {
            optimize: true,
            debug: false
        }
    // } else {
    //     return {
    //         optimize: false,
    //         debug: true
    //     }
    // }
}

const options = elm_options()

// the absWorkingDirectory set below allows us to use paths relative to that location
esbuild.build({
    entryPoints: ['./hello_elm.js'],
    bundle: true,
    outdir: path.join(process.cwd(), "app/assets/builds"),
    absWorkingDir: path.join(process.cwd(), "app/javascript"),
    // watch: process.argv.includes("--watch"),
    sourcemap: true,
    plugins: [
        ElmPlugin(options) // options are documented below
    ],
}).catch(e => (console.error(e), process.exit(1)))
