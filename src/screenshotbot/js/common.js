
function setupLiveOnAttach(selector, fn) {
    if (!window.liveAttachEvents) {
        window.liveAttachEvents = [];
    }

    window.liveAttachEvents.push([selector, fn])
    $(selector).map(fn);
}

function callLiveOnAttach(nodes) {
    console.log("calling live on attach", nodes, window.liveAttachEvents);
    $(nodes).map(function () {
        var node  = this;
        window.liveAttachEvents.forEach(function (ev) {
            $(ev[0], node).map(ev[1]);

            if ($(node).is(ev[0])) {
                ev[1].call(node);
            }
        });

    });
}

function prepareReportJs () {
    setupLiveOnAttach(".change-image-left", function () {
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
            if (!$.fx.off) {
                $(img).attr("src", src);
            }
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


    function setupImageComparison() {
        var img = $(".image-comparison-modal-image", this);
        var wrapper = $(img).closest(".progress-image-wrapper");
        var loading = $(wrapper).find(".loading");

        var zoomToChange = $(".zoom-to-change", this);
        var zoomToChangeSpinner = $(zoomToChange).find(".spinner-border");

        var src = img.data("src");

        resetImageZoom(img);

        zoomToChange.prop("disabled", true);
        loading.show();
        img.hide();
        $.ajax({
            url: src,
            success: function (data) {
                loading.hide();
                img.show();
                img.on("load", function () {
                    zoomToChange.prop("disabled", false);
                });

                if (data.identical) {
                    $(".images-identical", wrapper).show();
                }
                console.log("Comparison result" ,data);
                img.attr("src", data.src);
                img.css("background-image", "url(\"" + data.src + '")');
                img.css("background-repeat", "no-repeat");
            },
            error: function () {
                swAlert("Network request failed! This could be because we're still processing the image. Please try again in a minute");
            }
        });


        zoomToChange.click(function () {
            // move the image out of the way
            img.css("object-position", "10000px 10000px");
            img.css("background-size", "100%");

            zoomToChange.prop("disabled", true);
            zoomToChangeSpinner.show();

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
                    var pX = data.x / data.width;
                    var pY = data.y / data.height;

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
                    zoomToChangeSpinner.hide();

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
                    zoomToChangeSpinner.hide();
                }
            });
        });

    }


    setupLiveOnAttach(".image-comparison-modal", function () {
        $(this).on("show.bs.modal", setupImageComparison);
    });
    setupLiveOnAttach(
        ".image-comparison-wrapper",
        function () {
            var img = $(this);
            console.log("Setting up image comparison on: ", img);
            setupImageComparison.call(img);
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
