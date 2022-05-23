
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
