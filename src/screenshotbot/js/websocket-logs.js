
$("[data-websocket-stream]").each(function () {
    var $host = $(this);
    var url = $(this).data("websocket-stream");

    function append(line) {
        $host.append(line);

        $host.scrollTop($host[0].scrollHeight);
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
