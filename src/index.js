'use strict';

import './assets/style/main.scss';

var Elm = require('./Main.elm');
var mountNode = document.getElementById('elmapp');
var app = Elm.Main.embed(mountNode);
