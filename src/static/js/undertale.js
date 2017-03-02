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
undertale.ports.getImg.subscribe(function(svgId) {
    setTimeout(function() {

        var mySVG    = document.getElementById(svgId);      // Inline SVG element
        svgAsPngUri(mySVG, {}, function(uri) {
            undertale.ports.getRenderData.send(uri);
        });

        // The following doesn't inline images
        //     can      = document.createElement('canvas'), // Not shown on page
        //     ctx      = can.getContext('2d'),
        //     loader   = new Image;                        // Not shown on page
        //     var bbox     = mySVG.getBBox();
        //     var data;

        //     loader.width  = can.width  = bbox.width;
        //     loader.height = can.height = bbox.height;

        //     loader.onload = function(){
        //         console.log(loader.src);
        //         ctx.drawImage( loader, 0, 0, loader.width, loader.height );
        //         data = can.toDataURL("image/png");

        //         console.log(data);
        //         undertale.ports.getRenderData.send(data);
        //     };

        //     var svgAsXML = (new XMLSerializer).serializeToString( mySVG );
        //     loader.src = 'data:image/svg+xml,' + encodeURIComponent( svgAsXML );
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
