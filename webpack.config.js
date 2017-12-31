const path = require('path');
const webpack = require('webpack'); // for built-in plugins
const htmlWebpack = require('html-webpack-plugin'); // installed via npm
const elmLoader = require('elm-webpack-loader');
const fileLoader = require('file-loader');
const styleLoader = require('style-loader');
const cssLoader = require('css-loader');
const urlLoader = require('url-loader');
const devServer = require('webpack-dev-server');
const sassLoader = require('sass-loader');
const ExtractTextPlugin = require("extract-text-webpack-plugin");

distConfig = {
  name: "dist",
  entry: {
    app: [
      './src/index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: 'bundle.js',
  },

  module: {
    rules: [
      {
        test: /\.css$/,
        use: 'css-loader'
      },
      {
        test: /\.(scss|sass)$/,
        use: [{loader:'style-loader'}, {loader:'css-loader'}, {loader:'sass-loader'}]
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file-loader?name=[name].[ext]',
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader?verbose=true&warn=true',
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff',
      },
      {
        test: /\.(ttf|eot|svg|png|jpg|gif)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'file-loader',
      },
    ],
    noParse: /\.elm$/,
  },

  plugins: [
    new htmlWebpack({
      title: 'Family Tree',
      favicon: 'src/assets/favicon.png',
      template: 'src/index.ejs',
      filename: 'index.html'
    }),
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
  }
};

const extractor = new ExtractTextPlugin({
  allChunks: true,
  filename: 'main.css',
});

protoConfig = {
  name: "prototyping",

  entry: path.resolve(__dirname + '/src/assets/style/main.scss'),
  output: {
    path: path.resolve(__dirname + '/html/assets/style/generated'),
    filename: '__ignore.js',
  },

  module: {
    rules: [
      {
        test: /\.(scss|sass|css)$/,
        use: extractor.extract(
          ['css-loader', 'sass-loader']
        ),
      },
      {
        test: /\.(ttf|eot|svg|png|jpg|gif)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'file-loader',
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff',
      },
    ],
    noParse: /\.elm$/,
  },

  plugins: [extractor],
};

module.exports = [distConfig, protoConfig];
