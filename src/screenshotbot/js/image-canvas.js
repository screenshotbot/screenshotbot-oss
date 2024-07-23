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
        this.imgSize = {}

        this.images = null;

        this.drawTimeout = null;

        $(canvasContainer).data("image-canvas", this);

        /* constants?  At time of writing, unused. */

        var dpr = devicePixelRatio || 1;
        this.dpr = dpr;
        this.dprTransform = new DOMMatrix([dpr, 0, 0, dpr, 0, 0]);
        this.dprInv = this.dprTransform.inverse();
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

    animateTo(newTransform, callback) {
        var self = this;
        console.log("animating to: ", newTransform);
        var oldTransform = self.transform;
        $(self.canvasEl).animate(
            {fake:100},
            {
                duration: 1000,
                complete: callback,
                progress: function (animation, progress) {
                    self.transform = animateTransform(oldTransform, newTransform, progress);
                    self.scheduleDraw();
                },
            });
    }

    zoomToImagePx(data) {
        var self = this;
        console.log("data", data);
        var rect = self.canvasEl.getBoundingClientRect();
        var scale = Math.min(self.canvasEl.width / rect.width, self.canvasEl.height / rect.height);

        var newTransform = calcTransformForCenter(
            rect.width,
            rect.height,
            self.imgSize.width,
            self.imgSize.height,
            data.x,
            data.y,
            4 /* new zoom */);

        self.animateTo(newTransform, function () {
            console.log("animation done");
            self.callCallback(self.callbacks.onZoomComplete);
        });
    }



    updateCoreTransform() {

        let rect = this.canvasContainer.getBoundingClientRect();
        let scrollWidth = rect.width;
        let scrollHeight = rect.height;

        var canvasEl = this.canvasEl;
        var dpr = this.dpr;

        if (canvasEl.height != scrollHeight * dpr ||
            canvasEl.width != scrollWidth * dpr) {
            canvasEl.height = scrollHeight * dpr;
            canvasEl.width = scrollWidth * dpr;

            canvasEl.style.width = scrollWidth + 'px';
            canvasEl.style.height = scrollHeight + 'px';
        }

        this.coreTranslation = calcCoreTransform(scrollWidth,
                                                 scrollHeight,
                                                 this.imgSize.width,
                                                 this.imgSize.height);
        /*console.log("got core translation", this.coreTranslation,
          " for ", scrollWidth, scrollHeight, this.canvasEl.width,
          this.canvasEl.height); */
    }

    clearCtx() {
        this.ctx.setTransform(_identity);
        this.ctx.clearRect(0, 0, this.canvasEl.width, this.canvasEl.height);
        this.updateCoreTransform();
        this.updateTransform();
    }

    draw() {
        if (!this.images) {
            return;
        }

        var self = this;
        self.clearCtx();
        var ctx = self.ctx;

        function doDraw(image) {
            /* Disabling imageSmoothing is not great, I think. See T1295. */
            //ctx.imageSmoothingEnabled = false;
            ctx.imageSmoothingQuality = "high";
            if (image) {
                ctx.drawImage(image, 0, 0,
                              self.imgSize.width,
                              self.imgSize.height);
            }
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
                doDraw(self.images[i]);
            }
        }

        self.drawMasks();
    }

    scheduleDraw () {
        var self = this;
        if (!self.drawTimeout) {
            self.drawTimeout = setTimeout(function () {
                self.drawTimeout = null;
                self.draw();
            }, 16);
        }
    }

    onImagesLoaded() {
        var self = this;
        self.callCallback(self.callbacks.onImagesLoaded);

        // The last image determines the canvas size.
        var image = self.images[self.images.length - 1];

        self.imgSize = {
            height: image.height * self.dpr,
            width: image.width * self.dpr,
        }
        self.scheduleDraw();
    }


    getEventPositionOnCanvas(e) {
        var self = this;
        var rect = self.canvasEl.getBoundingClientRect();
        // Since we're using `cover` as object-fit, the scale
        // will be the higher of these two
        var scale = Math.min(self.canvasEl.width / rect.width, self.canvasEl.height / rect.height);
        console.log(e.clientX - rect.left, e.clientY - rect.top);
        var thisX = (e.clientX - rect.left) * scale;
        var thisY = (e.clientY - rect.top) * scale;

        //console.log(thisX, thisY);

        return new DOMPoint(thisX, thisY);
    }

    // subtract two points
    _subtract(a, b){
        return new DOMPoint(a.x - b.x,
                            a.y - b.y);
    }

    setupDragging() {
        var self = this;
        // Mapping from pointerId to { x: 0, y: 0, translateX: 0, translateY: 0 };
        var dragStart = {};

        function onMouseDown(e) {
            if (e.which != 1) {
                return;
            }

            var pointerId = e.pointerId;
            dragStart[pointerId] = {
                x0: self.getEventPositionOnCanvas(e),
                M0: DOMMatrix.fromMatrix(self.transform),
                startTime: Date.now(),
            }

            e.preventDefault();
        }

        function onMouseMove(e) {
            // See F215508 and F215510  for how we derived these equations.
            var pos = self.getEventPositionOnCanvas(e);
            var ds = dragStart[e.pointerId]
            if (ds && ds.startTime < Date.now() - 100) {

                var delta = self.dprInv.transformPoint(self._subtract(pos, ds.x0));

                //console.log("got delta: ", delta);
                self.transform.e = ds.M0.e + delta.x;
                self.transform.f = ds.M0.f + delta.y;

                self.scheduleDraw();
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

    }


    setupZoomWheel() {
        var self = this;
        function onZoomWheel(e) {
            var change = e.originalEvent.deltaY * 0.0005;
            var zoom0 = self.getZoom();
            var M0 = DOMMatrix.fromMatrix(self.transform);

            var zoom = zoom0 - change;

            if (zoom > 5) {
                zoom = 5;
            }

            if (zoom < 0.1) {
                zoom = 0.1;
            }
            //console.log("new zoom is", zoom);

            // But I want the mouse to be on the same location
            // that we started with. For this we need to move translate.x



            var x0 = self.getEventPositionOnCanvas(e);

            var Z = new DOMMatrix([zoom, 0, 0, zoom, 0, 0]);
            var delta = self._subtract(
                self.dprInv.transformPoint(x0),
                Z.multiply(M0.inverse()).multiply(self.dprInv).transformPoint(x0));

            self.transform = Z;
            self.transform.e = delta.x;
            self.transform.f = delta.y;


            self.scheduleDraw();
            e.preventDefault();

        }

        $(self.canvasEl).on("wheel", function (e) {
            onZoomWheel(e);
        });
    }

    load() {
        var self = this;

        console.log("loadIntoCanvas", self.canvasContainer, this.layers);

        updateResizeObserver(self.canvasContainer, new ResizeObserver((entries) => {
            self.scheduleDraw();
        }));

        var $canvas = $("<canvas draggable='false' class='load-into-canvas' style='touch-action:none; '/>");
        this.canvasEl = $canvas.get(0);

        $(self.canvasContainer).empty();
        self.canvasContainer.appendChild(self.canvasEl);

        /* If the window is resized, or image is reloated, this is the, first translation
           that happens independently of mouse zooms etc. */
        this.coreTranslation = _identity;

        self.transform = new DOMMatrix([1, 0, 0, 1, 0, 0]);

        self.images = [];

        self.image_promises = Promise.all(this.layers.map((layer) => {
            return fetch(layer.src)
                .then((response) => response.blob())
                .then(blob => createImageBitmap(blob));
        })).then((images) => {
            self.images = images;
            self.onImagesLoaded();
        });


        var ctx = self.canvasEl.getContext('2d');
        self.ctx = ctx;

        this.setupDragging();
        this.setupZoomWheel();

        $(self.canvasContainer).on("sb:refresh",
                                   () => self.scheduleDraw());

        function getEventPositionOnImage(e) {
            var canvasPos = self.getEventPositionOnCanvas(e);
            return canvasPos.matrixTransform(ctx.getTransform().inverse());
        }


        $(self.canvasEl).dblclick(function (e) {
            var imPos = getEventPositionOnImage(e);
            self.zoomToImagePx(imPos);
            e.preventDefault();
        });

        $(self.canvasEl).on("zoomToChange", function (e) {
            console.log("zoomToChange", e);
            var data = e.originalEvent.detail;

            self.zoomToImagePx(data);
        });
    }
}

// convenience for debugging()
function getSbCanvas() {
    return $("canvas").parent().data("image-canvas");
}
