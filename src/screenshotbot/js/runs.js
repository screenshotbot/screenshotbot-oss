

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

setupLiveOnAttach(".search", function () {
    $(this).on("input", function () {
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
});


setupLiveOnAttach("ul.report-selector > li > a", function () {
    $(this).click(function (e) {
        var $ul = $(this).closest("ul");
        $ul.find("a").removeClass("active");
        $(this).addClass("active");
        var $target = $($ul.data("target"));

        var type = $(this).data("type");

        var args = $target.data("args");

        args["type"] = type;

        updateAjaxResults($target);

        e.preventDefault();
    });

});
