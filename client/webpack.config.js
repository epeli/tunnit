module.exports = {
  entry: "./index.js",
  output: {
    path: __dirname + "/public",
      filename: "bundle.js",
      publicPath: "/"
  },
  module: {
    loaders: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      loader: 'elm-webpack'
    }]
  }
};
