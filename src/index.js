'use strict';

import './assets/style/main.scss';

var Elm = require('./Main.elm');
var app =
    Elm.Elm.Main.init({
        node: document.getElementById('elmapp')
    });