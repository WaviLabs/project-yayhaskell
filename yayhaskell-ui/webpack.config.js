const path = require('path');

module.exports = {
  entry: './src/animateGrid.js',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist'),
  },
};
