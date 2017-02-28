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

// http://stackoverflow.com/questions/2483919/how-to-save-svg-canvas-to-local-filesystem
undertale.ports.render.subscribe(function(params) {
    setTimeout(function() {
            // Add some critical information
            $("svg").attr({ version: '1.1' , xmlns:"http://www.w3.org/2000/svg"});

            var svg = $(params.svgId).html();
            var b64 = Base64.encode(svg); // or use btoa if supported

            // Works in recent Webkit(Chrome)
            $("body").append($("<img src='data:image/svg+xml;base64,\n"+b64+"' alt='file.svg'/>"));

            // Works in Firefox 3.6 and Webit and possibly any browser which supports the data-uri
            $("body").append($("<a href-lang='image/svg+xml' href='data:image/svg+xml;base64,\n"+b64+"' title='file.svg'>Download</a>"));
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
