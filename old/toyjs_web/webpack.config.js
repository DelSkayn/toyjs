const path = require('path');
const WasmPackPlugin = require('@wasm-tool/wasm-pack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

const distPath = path.resolve(__dirname, "dist");
module.exports = (env, argv) => {
  return {
    devServer: {
      contentBase: distPath,
      compress: argv.mode === 'production',
      port: 8000
    },
    entry: './index.js',
    output: {
      path: distPath,
      filename: "toyjs.js",
      webassemblyModuleFilename: "toyjs.wasm"
    },
    module: {
      rules: [
        {
          test: /\.css$/i,
          use: [
            'style-loader',
            'css-loader',
          ],
        },
      ],
    },
    plugins: [
      new CopyWebpackPlugin({
        patterns: [
          { from: './static', to: distPath },
        ],
      }),
      new WasmPackPlugin({
        crateDirectory: ".",
        extraArgs: "--no-typescript",
      })
    ],
    watch: argv.mode !== 'production'
  };
};
