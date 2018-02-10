'use strict';

module.exports = {
  entry: './src/app',
  output: {
    path: __dirname,
    pathinfo: true,
    filename: 'bundle.js'
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {
          src: [
            'bower_components/purescript-*/src/**/*.purs',
            'src/**/*.purs'
          ]
        }
      }
    ]
  },
  resolve: {
    extensions: ['.purs', '.js']
  }
};