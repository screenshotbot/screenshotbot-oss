class SbImageCanvas {
    constructor(canvasContainer,
                layers,
                masks,
                callbacks) {
        this.canvasContainer = canvasContainer;
        this.layers = layers;
        this.masks = masks;
        this.callbacks = callbacks;

        // Set later.
        this.ctx = null;
        this.transform = null;
        this.coreTranslation = null;
        this.canvasEl = null;
    }

    callCallback(fn) {
        if (fn) {
            fn();
        }
    }

    drawMasks() {
        var ctx = this.ctx;
        ctx.globalAlpha = 1;
        for (var mask of this.masks) {
            ctx.beginPath();

            ctx.rect(mask.left,
                     mask.top,
                     mask.width,
                     mask.height);
            ctx.fillStyle = "rgba(255, 255, 0, 0.8)";
            ctx.fill();
        }
    }

    setZoom(z) {
        this.transform.a = z;
        this.transform.d = z;
    }

    getZoom() {
        return this.transform.a;
    }

    setTranslate(x, y) {
        this.transform.e = x;
        this.transform.f = y;
    }

    updateTransform () {
        var mat = this.transform;
        var res = mat.multiply(this.coreTranslation);
        //console.log("new transform", res);
        this.ctx.setTransform(res);
    }

    load() {
        var self = this;

        console.log("loadIntoCanvas", self.canvasContainer, this.layers);

        updateResizeObserver(self.canvasContainer, new ResizeObserver((entries) => {
            scheduleDraw();
        }));

        var $canvas = $("<canvas draggable='false' class='load-into-canvas' style='touch-action:none; '/>");
        this.canvasEl = $canvas.get(0);

        $(self.canvasContainer).empty();
        self.canvasContainer.appendChild(self.canvasEl);

        let imgSize = {};

        /* If the window is resized, or image is reloated, this is the, first translation
           that happens independently of mouse zooms etc. */
        this.coreTranslation = _identity;

        self.transform = new DOMMatrix([1, 0, 0, 1, 0, 0]);

        /* At time of writing these three variables are unused */
        var dpr = devicePixelRatio || 1;
        var dprTransform = new DOMMatrix([dpr, 0, 0, dpr, 0, 0]);
        var dprInv = dprTransform.inverse();

        var images = [];
        for (let layer of this.layers) {
            var im = new Image();
            im.src = layer.src;
            images.push(im);
        }

        var ctx = self.canvasEl.getContext('2d');
        self.ctx = ctx;

        var drawTimeout = null;
        function scheduleDraw() {
            if (!drawTimeout) {
                drawTimeout = setTimeout(function () {
                    drawTimeout = null;
                    draw();
                }, 16);
            }
        }
        function clearCtx() {
            ctx.setTransform(_identity);
            ctx.clearRect(0, 0, self.canvasEl.width, self.canvasEl.height);
            updateCoreTransform();
            self.updateTransform();
        }

        function draw() {
            clearCtx();

            function doDraw(image) {
                /* Disabling imageSmoothing is not great, I think. See T1295. */
                //ctx.imageSmoothingEnabled = false;
                ctx.imageSmoothingQuality = "high";
                ctx.drawImage(image, 0, 0);
            }



            self.updateTransform();
            for(let i in self.layers) {

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

                ctx.globalAlpha = getAlpha(self.layers[i]);
                if (ctx.globalAlpha > 0) {
                    doDraw(images[i]);
                }
            }

            self.drawMasks();
        }

        var imageLoadCounter = 0;

        function onEitherImageLoad() {
            imageLoadCounter ++;
            if (imageLoadCounter == images.length) {
                self.callCallback(self.callbacks.onImagesLoaded);

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

            let rect = self.canvasContainer.getBoundingClientRect();
            let scrollWidth = rect.width;
            let scrollHeight = rect.height;

            var canvasEl = self.canvasEl;
            if (canvasEl.height != scrollHeight ||
                canvasEl.width != scrollWidth) {
                canvasEl.height = scrollHeight;
                canvasEl.width = scrollWidth;
            }

            self.coreTranslation = calcCoreTransform(scrollWidth,
                                                scrollHeight,
                                                imgSize.width,
                                                imgSize.height);
            /*console.log("got core translation", self.coreTranslation,
              " for ", scrollWidth, scrollHeight, self.canvasEl.width,
              self.canvasEl.height); */
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
            dragStart[pointerId] = getEventPositionOnCanvas(e);
            var ds = dragStart[pointerId];
            ds.translateX = self.transform.e;
            ds.translateY = self.transform.f;
            ds.startTime = Date.now();

            e.preventDefault();
        }

        function onMouseMove(e) {
            var pos = getEventPositionOnCanvas(e);
            var ds = dragStart[e.pointerId]
            if (ds && ds.startTime < Date.now() - 100) {
                self.transform.e = pos.x - ds.x + ds.translateX;
                self.transform.f = pos.y - ds.y + ds.translateY;
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
        $(self.canvasEl).on("pointerdown", onMouseDown);

        $(self.canvasEl).on("remove", function () {
            console.log("removing listeners");
            document.removeEventListener("pointermove", onMouseMove);
            document.removeEventListener("pointerup", onMouseEnd);
            document.removeEventListener("pointercancel", onMouseEnd);
        });

        $(self.canvasContainer).on("sb:refresh", scheduleDraw);

        function getEventPositionOnCanvas(e) {
            var rect = self.canvasEl.getBoundingClientRect();
            // Since we're using `cover` as object-fit, the scale
            // will be the higher of these two
            var scale = Math.min(self.canvasEl.width / rect.width, self.canvasEl.height / rect.height);

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
            var zoom0 = self.getZoom();

            self.setZoom(self.getZoom() - change);

            if (self.getZoom() > 5) {
                self.setZoom(5);
            }

            if (self.getZoom() < 0.1) {
                self.setZoom(0.1);
            }
            //console.log("new zoom is", zoom);

            // But I want the mouse to be on the same location
            // that we started with. For this we need to move translate.x



            var canvasPos = getEventPositionOnCanvas(e);

            var zoom = self.getZoom();
            var translate = {
                x: canvasPos.x - (zoom/zoom0) * (canvasPos.x - self.transform.e),
                y: canvasPos.y - (zoom/zoom0) * (canvasPos.y - self.transform.f)
            }

            self.setTranslate(translate.x, translate.y);

            scheduleDraw();
            e.preventDefault();

        }

        $(self.canvasEl).on("wheel", function (e) {
            onZoomWheel(e);
        });

        $(self.canvasEl).dblclick(function (e) {
            var imPos = getEventPositionOnImage(e);
            zoomToImagePx(imPos);
            e.preventDefault();
        });

        function animateTo(newTransform, callback) {
            console.log("animating to: ", newTransform);
            var oldTransform = self.transform;
            $(self.canvasEl).animate(
                {fake:100},
                {
                    duration: 1000,
                    complete: callback,
                    progress: function (animation, progress) {
                        self.transform = animateTransform(oldTransform, newTransform, progress);
                        scheduleDraw();
                    },
                });
        }

        function zoomToImagePx(data) {
            console.log("data", data);
            var rect = self.canvasEl.getBoundingClientRect();
            var scale = Math.min(self.canvasEl.width / rect.width, self.canvasEl.height / rect.height);

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
                self.callCallback(self.callbacks.onZoomComplete);
            });
        }

        $(self.canvasEl).on("zoomToChange", function (e) {
            console.log("zoomToChange", e);
            var data = e.originalEvent.detail;

            zoomToImagePx(data);
        });
    }
}
