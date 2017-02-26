var undertale = Elm.UndertaleDialog.fullscreen({
        staticRoot: $STATIC_ROOT,
        scriptRoot: $SCRIPT_ROOT
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

/*
function sendCursorPosition(params) {
    var ctl = document.getElementById(params.elementId);
    var endPos = ctl.selectionEnd;
    undertale.ports.cursorPos.send(endPos);
}

undertale.ports.input.subscribe(function(params) {
    sendCursorPosition(params);
})
*/
