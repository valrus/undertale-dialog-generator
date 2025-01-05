const { Elm } = require('./UndertaleDialog.elm');

var undertale = Elm.UndertaleDialog.init({
        node: document.getElementById('root'),
        flags: {
            staticRoot: './',
            scriptRoot: './'
        },
    }
);

undertale.ports.focus.subscribe(function(params) {
    setTimeout(function() {
        var elem = document.getElementById(params.elementId);
        if (elem) {
            elem.focus();
            if (params.moveCursorToEnd) {
                var val = elem.value;
                elem.value = '';
                elem.value = val;
            }
        }
    }, 50);
});
