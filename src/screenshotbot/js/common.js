
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

function loadIntoCanvas(canvasEl, layers, masks, callbacks) {
    function callCallback(fn) {
        if (fn) {
            fn();
        }
    }
    var translate = { x: 0, y: 0 };
    var zoom = 1;
    var masks = [];

    var images = [];
    for (let layer of layers) {
        var im = new Image();
        im.src = layer.src;
        images.push(im);
    }

    var ctx = canvasEl.getContext('2d');

    var drawTimeout = null;
    function scheduleDraw() {
        if (!drawTimeout) {
            drawTimeout = setTimeout(function () {
                drawTimeout = null;
                draw();
            }, 16);
        }
    }

    function draw() {
        //fixMaxTranslation();
        // x* = t + sx. x = (x* - t) / s
        function reverseMap(pos) {
            function r(x_star, t) {
                return (x_star - t) / zoom;
            }
            return {
                x: r(pos.x, translate.x),
                y: r(pos.y, translate.y)
            }
        }

        function forwardMap(pos) {
            function m(x, t) {
                return t + zoom * x;
            }

            return {
                x: m(pos.x, translate.x),
                y: m(pos.y, translate.y),
            }
        }

        var imTopLeft = reverseMap({x: 0, y: 0});
        var imBottomRight = reverseMap({
            x : canvasEl.width,
            y: canvasEl.height
        });
        ctx.clearRect(0, 0, canvasEl.width, canvasEl.height);

        /*
          console.log("Source prop", translate, imTopLeft.x, imTopLeft.y,
          imBottomRight.x - imTopLeft.x,
          imBottomRight.y - imTopLeft.y);
        */

        function doDraw(image) {
            ctx.drawImage(image,
                          imTopLeft.x, imTopLeft.y,
                          imBottomRight.x - imTopLeft.x,
                          imBottomRight.y - imTopLeft.y,
                          0, 0,
                          canvasEl.width,
                          canvasEl.height);
        }

        function drawMasks() {
            for (var mask of masks) {
                ctx.beginPath();

                var maskPos = forwardMap({
                    x: mask.left,
                    y: mask.top,
                });

                ctx.rect(maskPos.x,
                         maskPos.y,
                         mask.width * zoom,
                         mask.height * zoom);
                ctx.fillStyle = "rgba(255, 255, 0, 0.8)";
                ctx.fill();
            }
        }


        for(let i in layers) {
            ctx.globalAlpha = layers[i].alpha || 1;
            doDraw(images[i]);
        }

        drawMasks();
    }

    function fixMaxTranslation () {
        var rect = canvasEl.getBoundingClientRect();
        var scale = Math.min(canvasEl.width / rect.width, canvasEl.height / rect.height);

        if (translate.x >  rect.width * scale / 2) {
            translate.x = rect.width * scale / 2;
        }

        if (translate.y > rect.height * scale / 2) {
            translate.y = rect.height * scale / 2;
        }

        if (translate.x + canvasEl.width < rect.width * scale / 2) {
            translate.x = rect.width * scale / 2 - canvasEl.width;
        }

        if (translate.y + canvasEl.height < rect.height * scale / 2) {
            translate.y = rect.height * scale / 2 - canvasEl.height;
        }

    }

    var imageLoadCounter = 0;

    function onEitherImageLoad() {
        imageLoadCounter ++;
        if (imageLoadCounter == images.length) {
            callCallback(callbacks.onImagesLoaded);

            // The last image determines the canvas size.
            var image = images[images.length - 1];

            canvasEl.height = image.height;
            canvasEl.width = image.width;
            scheduleDraw();
        }
    }

    for (let im of images) {
        im.onload = onEitherImageLoad;
    }
    var dragStart = { x: 0, y: 0, translateX: 0, translateY: 0 };

    function onMouseDown(e) {
        if (e.which != 1) {
            return;
        }

        var isDragging = true;
        dragStart = getEventPositionOnCanvas(e);
        dragStart.translateX = translate.x;
        dragStart.translateY = translate.y;

        // Only start moving after 250ms. In the meantime, if
        // we double-click, then we'll cancel this
        var timer = setTimeout(function () {
            document.addEventListener("mousemove", onMouseMove);
        }, 100)

        function onMouseMove(e) {
            var pos = getEventPositionOnCanvas(e);
            if (isDragging) {
                translate.x = pos.x - dragStart.x + dragStart.translateX;
                translate.y = pos.y - dragStart.y + dragStart.translateY;
                scheduleDraw();
            }
        }

        function onMouseEnd(e) {
            isDragging = false;
            clearTimeout(timer);
            document.removeEventListener("mousemove", onMouseMove);
            document.removeEventListener("mouseup", onMouseEnd);
        }


        document.addEventListener("mouseup", onMouseEnd);
    }

    var mouseDownTimer;
    $(canvasEl).on("mousedown", onMouseDown);

    function getEventPositionOnCanvas(e) {
        var rect = canvasEl.getBoundingClientRect();
        // Since we're using `cover` as object-fit, the scale
        // will be the higher of these two
        var scale = Math.min(canvasEl.width / rect.width, canvasEl.height / rect.height);

        /*console.log("here's what we're looking at", rect.width, canvasEl.width, translate.x);
          console.log("here's what we're looking at", rect.height, canvasEl.height, translate.y);
          console.log("But got scale", scale);*/
        var thisX = (e.clientX - rect.left) * scale;
        var thisY = (e.clientY - rect.top) * scale;

        return {
            x: thisX,
            y: thisY,
        };
    }

    function getEventPositionOnImage(e) {
        var canvasPos = getEventPositionOnCanvas(e);
        return {
            x: (canvasPos.x - translate.x) / zoom,
            y: (canvasPos.y - translate.y) / zoom,
        };
    }

    function onZoomWheel(e) {
        var change = e.originalEvent.deltaY * 0.0005;
        var zoom0 = zoom;

        zoom -= change;
        if (zoom > 5) {
            zoom = 5;
        }

        if (zoom < 0.1) {
            zoom = 0.1;
        }
        //console.log("new zoom is", zoom);

        // But I want the mouse to be on the same location
        // that we started with. For this we need to move translate.x



        var canvasPos = getEventPositionOnCanvas(e);

        translate = {
            x: canvasPos.x - (zoom/zoom0) * (canvasPos.x - translate.x),
            y: canvasPos.y - (zoom/zoom0) * (canvasPos.y - translate.y)
        }

        scheduleDraw();
        e.preventDefault();

    }

    $(canvasEl).on("wheel", function (e) {
        onZoomWheel(e);
    });

    $(canvasEl).dblclick(function (e) {
        var imPos = getEventPositionOnImage(e);
        zoomToImagePx(imPos);
        e.preventDefault();
    });

    function animateTo(newTranslate, newZoom, callback) {
        var oldTranslate = translate;
        var oldZoom = zoom;
        $(canvasEl).animate(
            {fake:100},
            {
                duration: 1000,
                complete: callback,
                progress: function (animation, progress) {
                    translate = {
                        x: (1-progress) * oldTranslate.x + progress * newTranslate.x,
                        y: (1-progress) * oldTranslate.y + progress * newTranslate.y
                    }

                    zoom = (1-progress)*oldZoom + progress * newZoom;
                    scheduleDraw();
                },
            });
    }

    function zoomToImagePx(data) {
        console.log("data", data);
        var rect = canvasEl.getBoundingClientRect();
        var scale = Math.min(canvasEl.width / rect.width, canvasEl.height / rect.height);

        var zoom = 4;
        var outputPixel = { x : scale * rect.width / 2, y: scale * rect.height / 2}
        /*
          The final position is t + sx for image position
          x. We want x to map to outputPixel. So we can get t
          = outputPixel - sx.
        */

        var newTranslate = {
            x: outputPixel.x - zoom * data.x,
            y: outputPixel.y - zoom * data.y,
        }

        console.log("redrawing ", translate, zoom);
        animateTo(newTranslate, zoom, function () {
            console.log("animation done");
            callCallback(callbacks.onZoomComplete);
        });
    }

    $(canvasEl).on("zoomToChange", function (e) {
        console.log("zoomToChange", e);
        var data = e.originalEvent.detail;

        zoomToImagePx(data);
    });
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
        var canvas = $("canvas.image-comparison-modal-image",this);

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


        $.ajax({
            url: src,
            success: function (data) {
                loading.hide();
                img.show();
                zoomToLink = data.zoomTo;
                console.log("Loading into canvas", data);
                loadIntoCanvas(canvas.get(0),
                               [{
                                   alpha: 0.2,
                                   src: data.background,
                               },
                                {
                                    alpha: 1,
                                    src: data.src,
                                }],
                               data.masks, {
                    onImagesLoaded: function () {
                        zoomToChange.prop("disabled", false);
                    },
                    onZoomComplete: function () {
                        zoomToChange.prop("disabled", false);
                    },
                });

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

            $.ajax({
                url: zoomToLink,
                success: function(data) {
                    var event = new CustomEvent("zoomToChange", { detail: data });
                    canvas.get(0).dispatchEvent(event);
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
            canvas.off("wheel");
            canvas.off("mousedown");
            canvas.off("mousemove");
            canvas.off("mouseup");
            canvas.off("zoomToChange");
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
        $.ajax({
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
