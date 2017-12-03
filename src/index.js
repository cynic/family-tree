'use strict';

import './assets/main.scss';

var Elm = require('./Main.elm');
var mountNode = document.getElementById('elmapp');
var app = Elm.Main.embed(mountNode);
