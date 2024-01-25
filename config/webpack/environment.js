// work around breaking change in Node 17.x
process.env.NODE_OPTIONS = "--openssl-legacy-provider"

const { environment, webpackConfig } = require('shakapacker')
const elm =  require('./loaders/elm')

environment.loaders.prepend('elm', elm)
module.exports = environment
