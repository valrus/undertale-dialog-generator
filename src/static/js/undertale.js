var undertale = Elm.fullscreen(
    Elm.UndertaleDialog,
    {
        staticRoot: $STATIC_ROOT,
        scriptRoot: $SCRIPT_ROOT
    }
);

// passing in above doesn't seem to work?
undertale.ports.staticRoot.send($STATIC_ROOT);
undertale.ports.scriptRoot.send($SCRIPT_ROOT);

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
