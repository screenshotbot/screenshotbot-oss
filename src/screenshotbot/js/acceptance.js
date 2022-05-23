
$(".review-list a").click(function (e) {

    var href = $(this).attr("href");

    $.ajax({
        url: href,
        error: function () {
            swAlert("Something went wrong, please refresh the page and try again");
        },
        success: function(data){
            var html = $(data);
            $(".review-panel").html(html);
            callLiveOnAttach($(".review-panel"));
        }
    });

    e.preventDefault();
});


setupLiveOnAttach(".review-input", function () {
    var reviewInput = $(this);
    function replace(href) {
        $.ajax({
            url: href,
            error: function () {
                swAlert("Something went wrong, please refresh the page and try again");
            },
            success: function(data) {
                var html = $(data);
                reviewInput.replaceWith(html);
                callLiveOnAttach(html);
            }
        });
    };
    $(".accept-link", $(this)).click(function (e) {
        replace($(this).attr("href"));
        e.preventDefault();
    });

    $(".reject-link", $(this)).click(function (e) {
        replace($(this).attr("href"));
        e.preventDefault();
    });

});
