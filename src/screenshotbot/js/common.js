$sb = {
    ajax: function(data) {
        let updated = {
            ...data,
            error: function (xmlHttpRequest) {
                if (xmlHttpRequest.status == 410) {
                    alert("This page has expired. Please refresh and try again.");
                } else {
                    if (data.error) {
                        data.error(xmlHttpRequest);
                    }
                }
            }
        }

        console.log("Using", updated);
        $.ajax(updated);
    }
}

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

let _identity = new DOMMatrixReadOnly([1,0,0,1,0,0]);


function updateResizeObserver(el, obs) {
    var key = "r-obs";
    var prevObserver = $(el).data(key);
    if (prevObserver) {
        prevObserver.disconnect();
    }
    $(el).data(key, obs);
    obs.observe(el);
}




function prepareReportJs () {
    setupLiveOnAttach(".change-image-left", function () {
        //console.log("Setting up mouseover", this);
        var img = $(this);
        var oldSrc = $(this).attr("src");
        var newImg = $(this).closest(".change-image-row")
            .find(".change-image-right");
        var newSrc = newImg.attr("src");

        function findSourceTag(img) {
            return $(img).closest("picture").find("source[type='image/webp'");
        }

        var oldWebpSrcset = findSourceTag(this).attr("srcset");
        var newWebpSrcset = findSourceTag(newImg).attr("srcset");

        var isTouching = false;
        var isMousing = false;

        function setImg() {
            let [src, webpSrcset] =
                (isTouching || isMousing)
                ? [newSrc, newWebpSrcset]
                : [oldSrc, oldWebpSrcset];
            if (src === undefined) {
                throw 'No src available';
            }

            if (webpSrcset ==- undefined) {
                throw 'No webp srcset available';
            }

            if (!$.fx.off) {
                $(img).attr("src", src);
                findSourceTag(img).attr("srcset", webpSrcset);
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
        var modal = this;
        var img = $(".image-comparison-modal-image", this);
        var canvasContainer = $(".canvas-container",this);

        var wrapper = $(img).closest(".progress-image-wrapper");
        var loading = $(wrapper).find(".loading");


        var zoomToLink = undefined; // loaded from comparison nibble
        var zoomToChange = $(".zoom-to-change", this);
        var zoomToChangeSpinner = $(zoomToChange).find(".spinner-border");

        var src = img.data("src");
        resetImageZoom(img);

        zoomToChange.prop("disabled", true);
        loading.show();
        img.hide();

        var beforeAlpha = 0.2;
        var diffAlpha = 1;
        var afterAlpha = 0;


        function refresh() {
            $(canvasContainer).trigger("sb:refresh");
        }

        function resetMenu() {
            $(".view-item").removeClass("fw-bold");
        }
        resetMenu();

        function setupViewItem(name, before, diff, after) {
            return $(name, modal).click((e) => {
                console.log("Switching to updated view");
                beforeAlpha = before;
                diffAlpha = diff;
                afterAlpha = after;
                refresh();
                resetMenu();
                $(name, modal).addClass("fw-bold");
                e.preventDefault();
            }).css("cursor", "pointer");
        }

        setupViewItem(".view-updated", 0, 0, 1);
        setupViewItem(".view-diff", 0.2, 1, 0)
            .addClass("fw-bold");
        setupViewItem(".view-previous", 1, 0, 0);

        $sb.ajax({
            url: src,
            success: function (data) {
                loading.hide();
                img.show();
                zoomToLink = data.zoomTo;
                metricsLink = data.metrics;
                $(".metrics-link", modal).attr("href", metricsLink);

                console.log("Loading into canvas", data);
                new SbImageCanvas(
                    canvasContainer.get(0),
                    [{
                        alpha: () => beforeAlpha,
                        src: data.background,
                    },
                     {
                         alpha: () => diffAlpha,
                         src: data.src,
                     },
                     {
                         alpha: () => afterAlpha,
                         src: data.afterImage,
                     }
                    ],
                    data.masks, {
                        onImagesLoaded: function () {
                            zoomToChange.prop("disabled", false);
                        },
                        onZoomComplete: function () {
                            zoomToChange.prop("disabled", false);
                        },
                    }).load();

            },
            error: function () {
                swAlert("Network request failed! This could be because we're still processing the image. Please try again in a minute");
            }
        });

        zoomToChange.on("click", function (e) {
            console.log("zoom to change clicked");
            // move the image out of the way

            zoomToChange.prop("disabled", true);
            zoomToChangeSpinner.show();

            $sb.ajax({
                url: zoomToLink,
                success: function(data) {
                    if (data.x < 0) {
                        alert("Both images are identical in content");
                    } else {
                        var event = new CustomEvent("zoomToChange", { detail: data });
                        $("canvas", canvasContainer).get(0).dispatchEvent(event);
                    }
                    zoomToChangeSpinner.hide();
                },
                error: function () {
                    alert("We couldn't find changed pixels! This is likely a bug on our end.");
                    zoomToChange.prop("disabled", false);
                    zoomToChangeSpinner.hide();
                }
            });

            e.preventDefault();
        });

        $(modal).on("hide.bs.modal", function () {
            zoomToChange.off("click");
        });

    }


    setupLiveOnAttach(".image-comparison-modal", function () {
        $(this).on("show.bs.modal", setupImageComparison);
    });
    setupLiveOnAttach(
        ".image-comparison-wrapper",
        function () {
            var img = $(this);
            //console.log("Setting up image comparison on: ", img);
            setupImageComparison.call(img);
        });
}

$(document).on("click", "a.screenshot-run-image", function (e) {
    e.preventDefault();
    var modal = $($(this).data("target"));
    var canvas = $(".canvas-container", modal);

    var src = canvas.data("src");
    canvas.data("image-number", $(this).data("image-number"));

    (new Modal(modal.get(0), {})).show();
});

$(document).on("show.bs.modal", ".single-screenshot-modal", function () {
    console.log("callback called");
    var title = $(this).find(".modal-title");
    var canvas = $(this).find(".canvas-container");
    var next = $(this).find(".next");
    var prev = $(this).find(".previous");
    var page = $(this).find(".page-num");

    next.unbind();
    prev.unbind();

    function fetchData() {
        var n = $(canvas).data("image-number");
        var src = $(canvas).data("src");
        $sb.ajax({
            url: src,
            data: {
                n: n,
            },
            error: function () {
                swAlert("Something went wrong, please try refreshing the page");
            },
            success: function (data) {
                updateData(data);
            }

        });
    }

    function updateData(data) {
        $(title).text(data.title);
        new SbImageCanvas(canvas.get(0),
                          [{
                              alpha: 1,
                              src: data.src,
                          }],
                          [], {}).load();
    }

    function setEnabled(link, enabled) {
        if (enabled) {
            link.removeClass("disabled");
        } else {
            link.addClass("disabled");
        }
    }

    function updateN(fn) {
        var num = $(canvas).data("image-number");
        var length = $(canvas).data("length");
        num = fn(num);

        page.html("" + (num + 1) + "/" + length);

        setEnabled(prev, num > 0);
        setEnabled(next, num < length - 1);

        $(canvas).data("image-number", num);
        fetchData();
    }

    next.on("click", function () {
        updateN((n) => n+1);
    });

    prev.on("click", function () {
        updateN((n) => n-1);
    });

    // Hacky way of making sure both the initializer and the Next/prev
    // use the same code paths.
    updateN((x) => x);
});


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

setupLiveOnAttach(".baguetteBox", function () {
    prepareBaguetteBox(this);
});


function prepareBaguetteBox(el) {
    if (typeof baguetteBox !== 'undefined') {
        var id = $(el).attr("id");
        if (id) {
            baguetteBox.run("#" + id);
        } else {
            console.log("Baguettebox didn't have an id: ", el);
        }
    }
}

setupLiveOnAttach(".modal-link", function () {
    $(this).click(function (e) {
        var href = $(this).data("href");
        $sb.ajax({
            url: href,
            error: function () {
                swAlert("Something went wrong. Please reload and try again");
            },
            success: function (data) {
                console.log("got data", data);
                var modal = $(data);
                $("body").append(modal);
                (new Modal(modal.get(0), {})).show();
                $(modal).on("data.bs.dismiss", function () {
                    $(modal).remove();
                });
            },
        });
        e.preventDefault();
    });
});


function setUrlParameter(key, value) {
    var url = new URL(window.location);
    url.searchParams.set(key, value);
    history.replaceState(null, null, url.href);
}
