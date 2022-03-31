

function updateAjaxResults($target) {
    $.ajax({
        url: $target.data("update"),
        data: $target.data("args"),
        success: function (result) {
            $target.html(result);
            callLiveOnAttach($target);
        },
        error: function () {
            alert("Something went wrong");
        },
    });
}

$(".search").on("input", function () {
    var val = $(this).val();
    var $target = $($(this).data("target"));

    var params = $target.data("args");

    params["search"] = val;
    var timeout = $target.data("timeout");
    if (timeout) {
        clearTimeout(timeout);
    }

    $target.data("timeout", setTimeout(function () {
        updateAjaxResults($target);
    }, 250));
});
