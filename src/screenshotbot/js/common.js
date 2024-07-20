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

function loadIntoCanvas(canvasContainer, layers, masks, callbacks) {
    console.log("loadIntoCanvas", canvasContainer, layers);
    function callCallback(fn) {
        if (fn) {
            fn();
        }
    }

    updateResizeObserver(canvasContainer, new ResizeObserver((entries) => {
        scheduleDraw();
    }));

    var $canvas = $("<canvas draggable='false' style='touch-action:none; '/>");
    var canvasEl = $canvas.get(0);

    $(canvasContainer).empty();
    canvasContainer.appendChild(canvasEl);

    let imgSize = {};

    /* If the window is resized, or image is reloated, this is the, first translation
       that happens independently of mouse zooms etc. */
    var coreTranslation = _identity;

    var transform = new DOMMatrix([1, 0, 0, 1, 0, 0]);

    function setZoom(z) {
        transform.a = z;
        transform.d = z;
    }

    function getZoom() {
        return transform.a;
    }

    function setTranslate(x, y) {
        transform.e = x;
        transform.f = y;
    }

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
    function updateTransform () {
        var mat = transform;
        var res = mat.multiply(coreTranslation);
        //console.log("new transform", res);
        ctx.setTransform(res);
    }

    function clearCtx() {
        ctx.setTransform(_identity);
        ctx.clearRect(0, 0, canvasEl.width, canvasEl.height);
        updateCoreTransform();
        updateTransform();
    }

    function draw() {
        clearCtx();

        function doDraw(image) {
            /* We used to have ctx.imageSmoothingQuality =
             * "high". Removed because of T1295, but I'm willing to
             * change my mind again. */
            ctx.imageSmoothingEnabled = false;
            ctx.drawImage(image, 0, 0);
        }

        function drawMasks() {
            ctx.globalAlpha = 1;
            for (var mask of masks) {
                ctx.beginPath();

                ctx.rect(mask.left,
                         mask.top,
                         mask.width,
                         mask.height);
                ctx.fillStyle = "rgba(255, 255, 0, 0.8)";
                ctx.fill();
            }
        }


        updateTransform();
        for(let i in layers) {

            function getAlpha(layer) {
                var alpha = layer.alpha;
                if (alpha === 0) {
                    return 0;
                } else if (!alpha) {
                    return 1;
                } else if ((typeof alpha) === "number") {
                    return alpha;
                } else {
                    return alpha();
                }
            }

            ctx.globalAlpha = getAlpha(layers[i]);
            if (ctx.globalAlpha > 0) {
                doDraw(images[i]);
            }
        }

        drawMasks();
    }

    function fixMaxTranslation () {
        var rect = canvasEl.getBoundingClientRect();
        var scale = Math.min(canvasEl.width / rect.width, canvasEl.height / rect.height);

        if (transform.e >  rect.width * scale / 2) {
            transform.e = rect.width * scale / 2;
        }

        if (transform.f > rect.height * scale / 2) {
            transform.f = rect.height * scale / 2;
        }

        if (transform.e + canvasEl.width < rect.width * scale / 2) {
            transform.e = rect.width * scale / 2 - canvasEl.width;
        }

        if (transform.f + canvasEl.height < rect.height * scale / 2) {
            transform.f = rect.height * scale / 2 - canvasEl.height;
        }

    }

    var imageLoadCounter = 0;

    function onEitherImageLoad() {
        imageLoadCounter ++;
        if (imageLoadCounter == images.length) {
            callCallback(callbacks.onImagesLoaded);

            // The last image determines the canvas size.
            var image = images[images.length - 1];

            imgSize = {
                height: image.height,
                width: image.width,
            }
            scheduleDraw();
        }
    }

    function updateCoreTransform() {

        let rect = canvasContainer.getBoundingClientRect();
        let scrollWidth = rect.width;
        let scrollHeight = rect.height;

        let newCanvasHeight = scrollHeight * devicePixelRatio;
        let newCanvasWidth = scrollWidth * devicePixelRatio;

        if (canvasEl.height != newCanvasWidth ||
            canvasEl.width != newCanvasWidth) {
            canvasEl.height = newCanvasHeight;
            canvasEl.width = newCanvasWidth;
        }

        coreTranslation = calcCoreTransform(scrollWidth,
                                            scrollHeight,
                                            imgSize.width,
                                            imgSize.height);
        /*console.log("got core translation", coreTranslation,
                    " for ", scrollWidth, scrollHeight, canvasEl.width,
                   canvasEl.height); */
    }


    for (let im of images) {
        im.onload = onEitherImageLoad;
    }
    // Mapping from pointerId to { x: 0, y: 0, translateX: 0, translateY: 0 };
    var dragStart = {};

    function onMouseDown(e) {
        if (e.which != 1) {
            return;
        }

        var pointerId = e.pointerId;

        dragStart[pointerId] = {x: e.clientX, y: e.clientY };

        ds = dragStart[pointerId];
        ds.translateX = transform.e;
        ds.translateY = transform.f;
        ds.startTime = Date.now();

        e.preventDefault();
    }

    function onMouseMove(e) {
        var ds = dragStart[e.pointerId]
        if (ds && ds.startTime < Date.now() - 100) {
            // Remember the transform is just transforming it so that
            // we're measured to the canvas size (by default), and the
            // translation in the transform matrix are after
            // scaling. So we don't need to work with Canvas
            // coordinates and instead screen coordinates is good
            // enough.

            transform.e = (e.clientX - ds.x) + ds.translateX ;
            transform.f = (e.clientY - ds.y) + ds.translateY ;
            scheduleDraw();
        }

        if (ds) {
            e.preventDefault();
        }
    }

    function onMouseEnd(e) {
        if (dragStart[e.pointerId]) {
            delete dragStart[e.pointerId];
            e.preventDefault();
        }
    }

    document.addEventListener("pointermove", onMouseMove);
    document.addEventListener("pointerup", onMouseEnd);
    document.addEventListener("pointercancel", onMouseEnd);
    $(canvasEl).on("pointerdown", onMouseDown);

    $(canvasEl).on("remove", function () {
        console.log("removing listeners");
        document.removeEventListener("pointermove", onMouseMove);
        document.removeEventListener("pointerup", onMouseEnd);
        document.removeEventListener("pointercancel", onMouseEnd);
    });

    $(canvasContainer).on("sb:refresh", scheduleDraw);

    function getEventPositionOnCanvas(e) {
        var rect = canvasEl.getBoundingClientRect();
        // Since we're using `cover` as object-fit, the scale
        // will be the higher of these two
        var scale = Math.min(canvasEl.width / rect.width, canvasEl.height / rect.height);

        var thisX = (e.clientX - rect.left) * scale;
        var thisY = (e.clientY - rect.top) * scale;

        return new DOMPoint(thisX, thisY);
    }

    function getEventPositionOnImage(e) {
        var canvasPos = getEventPositionOnCanvas(e);
        return canvasPos.matrixTransform(ctx.getTransform().inverse());
    }

    function onZoomWheel(e) {
        var change = e.originalEvent.deltaY * 0.0005;
        var zoom0 = getZoom();

        setZoom(getZoom() - change);

        if (getZoom() > 5) {
            setZoom(5);
        }

        if (getZoom() < 0.1) {
            setZoom(0.1);
        }
        //console.log("new zoom is", zoom);

        // But I want the mouse to be on the same location
        // that we started with. For this we need to move translate.x



        var canvasPos = getEventPositionOnCanvas(e);

        var zoom = getZoom();
        var translate = {
            x: canvasPos.x - (zoom/zoom0) * (canvasPos.x - transform.e),
            y: canvasPos.y - (zoom/zoom0) * (canvasPos.y - transform.f)
        }

        setTranslate(translate.x, translate.y);

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

    function animateTo(newTransform, callback) {
        console.log("animating to: ", newTransform);
        var oldTransform = transform;
        $(canvasEl).animate(
            {fake:100},
            {
                duration: 1000,
                complete: callback,
                progress: function (animation, progress) {
                    transform = animateTransform(oldTransform, newTransform, progress);
                    scheduleDraw();
                },
            });
    }

    function zoomToImagePx(data) {
        console.log("data", data);
        var rect = canvasEl.getBoundingClientRect();
        var scale = Math.min(canvasEl.width / rect.width, canvasEl.height / rect.height);

        var newTransform = calcTransformForCenter(
            rect.width,
            rect.height,
            imgSize.width,
            imgSize.height,
            data.x,
            data.y,
            4 /* new zoom */);

        animateTo(newTransform, function () {
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
                loadIntoCanvas(canvasContainer.get(0),
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
        loadIntoCanvas(canvas.get(0),
                       [{
                           alpha: 1,
                           src: data.src,
                       }],
                       [], {});
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
