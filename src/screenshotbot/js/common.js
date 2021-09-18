function prepareReportJs () {
    $(".change-image-left").map(function () {
        console.log("Setting up mouseover");
        var img = $(this);
        var oldSrc = $(this).attr("src");
        var newSrc = $(this).closest(".change-image-row")
            .find(".change-image-right").attr("src");

        var isTouching = false;
        var isMousing = false;

        function setImg() {
            var src = (isTouching || isMousing) ? newSrc : oldSrc;
            if (src === undefined) {
                throw 'No src available';
            }
            $(img).attr("src", src);
        }

        $(this).mouseover(function () {
            isMousing = true;
            setImg();
        });

        $(this).mouseout(function () {
            isMousing = false;
            setImg();
        });

        $(this).contextmenu(function () {
            isMousing = false;
            setImg();
        });

        $(this).on("touchstart", function (){
            isTouching = true;
            setImg();
            return false;
        });

        $(this).on("touchend", function () {
            isTouching = false;

            // This is a lie, because on mobile, we still want the
            // touchend to look like a mouseout
            isMousing = false;
            setImg();
            return false;
        });
    });

    function resetImageZoom(img) {
        img.stop();
        img.css("background-image", "none");
        img.css("object-position", "0px 0px");
        img.css("background-size", "100%");
        img.css("background-position", "0 0");
    }

    $(".image-comparison-modal").on('show.bs.modal', function () {
        var img = $(".image-comparison-modal-image", this);
        var zoomToChange = $(".zoom-to-change", this);

        var src = img.data("src");

        resetImageZoom(img);

        img.attr("src", src);
        img.css("background-image", "url(\"" + src + '")');
        img.css("background-repeat", "no-repeat");


        zoomToChange.click(function () {
            // move the image out of the way
            img.css("object-position", "10000px 10000px");
            img.css("background-size", "100%");

            zoomToChange.prop("disabled", true);

            $.ajax({
                url: img.data("zoom-to"),
                success: function(data) {
                    console.log("got data", data);
                    // let's begin an animation to (data.x, data.y)

                    // TODO: auto calculate maxScale
                    var maxScale = 10;

                    var zoom = 400;

                    var clientWidth = img.get(0).naturalWidth;
                    var clientHeight = img.get(0).naturalHeight;

                    console.log("clientWidth/Height", clientWidth, clientHeight);
                    console.log("width/height", img.width(), img.height());

                    // Percent X, and percent y
                    var pX = data.x / clientWidth;
                    var pY = data.y / clientHeight;

                    console.log("pX, pY", pX, pY);


                    // what's the percentage of the image that's shown
                    // in the final position? This is straightforward:
                    var pFinalWidth = 100 / zoom;
                    var pFinalHeight = 100 / zoom;

                    console.log("pFinalWidth/Height", pFinalWidth, pFinalHeight);

                    // So we can now calculate the left and top in
                    // percentages of the image.
                    var pLeft = pX - pFinalWidth / 2;
                    var pTop = pY - pFinalHeight / 2;

                    console.log("pLeft/Top", pLeft, pTop);

                    // And we can use that to bring it back to pixel
                    // land:
                    var finalLeft = - (pX * zoom / 100 - 0.5) * img.width();
                    var finalTop = - (pY * zoom / 100 - 0.5) * img.height();

                    console.log("finalLeft/Top", finalLeft, finalTop);

                    img.animate(
                        {
                            "background-size": zoom + "%",
                            "background-position-x": finalLeft + "px",
                            "background-position-y": finalTop + "px",
                        },
                        {
                            duration: 2000,
                            complete: function() {
                                zoomToChange.prop("disabled", false);
                            }
                        }
                    );
                },
                error: function () {
                    alert("Something went wrong");
                    zoomToChange.prop("disabled", false);
                }
            });
        });

    });
}

prepareReportJs();

function setupHeadroom() {
    var myElement = document.querySelector(".headroom");
    // construct an instance of Headroom, passing the element

    if (myElement !== null) {
        var headroom = new Headroom(myElement, {
        });
        // initialise
        headroom.init();
    }
}

$(setupHeadroom);

/*
AOS.init ({
    disable: function () {
        //return window.innerWidth < 1200;
        return false;
    }
});*/
