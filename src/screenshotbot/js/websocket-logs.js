
$("[data-websocket-stream]").each(function () {
    var $host = $(this);
    var url = $(this).data("websocket-stream");
    var initialStreamDone = false;

    var timeout  = -1;

    // Please keep this in sync with +len+ on remote.lisp
    var maxBufferSize = 1024;

    function append(line) {
        $host.append(document.createTextNode(line));

        // Minor optimization to avoid super duper aggressive
        // scrolling.
        if (line.length < maxBufferSize) {
            $host.scrollTop($host[0].scrollHeight);
        }
    }

    var socket = new WebSocket(url);
    socket.onerror = function (e) {
        console.log("Error reading socket", e);
        append("websocket error");
    };

    socket.onopen = function () {
        console.log("websocket opened");


    }

    socket.onmessage = function (e) {
        append(e.data);
    }
});
