function getOriginalPoint(canvas, point) {
    var matrix = canvas.viewportTransform;
    var inverse = fabric.util.invertTransform(matrix);
    return fabric.util.transformPoint(point, inverse);
}

function prepareMaskEditor() {
    var ctr = 0;
    function makeIm(im) {
        var callback = function () {
            ctr ++;

            if (ctr == 2) {
                console.log("everything is ready", img, overlay);
                prepareMaskEditorHelper(img, overlay);
            }
        }

        if (!im || im.complete) {
            console.log("image was already available");
            callback();
        } else {
            console.log("waiting in the background");
            im.onload = callback;
        }
        return im;
    }

    var img = makeIm(document.getElementById("mask-editor-image"));
    var overlay = makeIm(document.getElementById("mask-editor-overlay"));
}


function prepareMaskEditorHelper(img, overlay) {
    // Take from: http://fabricjs.com/custom-control-render
    var deleteIcon = "data:image/svg+xml,%3C%3Fxml version='1.0' encoding='utf-8'%3F%3E%3C!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'%3E%3Csvg version='1.1' id='Ebene_1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' width='595.275px' height='595.275px' viewBox='200 215 230 470' xml:space='preserve'%3E%3Ccircle style='fill:%23F44336;' cx='299.76' cy='439.067' r='218.516'/%3E%3Cg%3E%3Crect x='267.162' y='307.978' transform='matrix(0.7071 -0.7071 0.7071 0.7071 -222.6202 340.6915)' style='fill:white;' width='65.545' height='262.18'/%3E%3Crect x='266.988' y='308.153' transform='matrix(0.7071 0.7071 -0.7071 0.7071 398.3889 -83.3116)' style='fill:white;' width='65.544' height='262.179'/%3E%3C/g%3E%3C/svg%3E";

    var delImg = document.createElement('img');
    delImg.src = deleteIcon;


    fabric.Object.prototype.controls.deleteControl = new fabric.Control({
        x: 0.5,
        y: -0.5,
        offsetY: 16,
        cursorStyle: 'pointer',
        mouseUpHandler: deleteObject,
        render: renderIcon,
        cornerSize: 24
    });


    function deleteObject(eventData, transform) {
		var target = transform.target;
		var canvas = target.canvas;
		canvas.remove(target);
        canvas.requestRenderAll();
	}

    function renderIcon(ctx, left, top, styleOverride, fabricObject) {
        var size = this.cornerSize;
        ctx.save();
        ctx.translate(left, top);
        ctx.rotate(fabric.util.degreesToRadians(fabricObject.angle));
        ctx.drawImage(delImg, -size/2, -size/2, size, size);
        ctx.restore();
    }

    console.log("Setting up mask editor");
    var canvas = new fabric.Canvas("mask-editor");
    var imgInstance = new fabric.Image(img, {
        left: 0,
        top: 0,
        opacity: 0.7,
        selectable: false,
    });


    canvas.add(imgInstance);

    if (overlay) {
        var overlayInstance = new fabric.Image(overlay, {
            left: 0,
            top: 0,
            selectable: false,
        });
        canvas.add(overlayInstance);
    }


    canvas.on('selection:created', function (options) {
        console.log("options: ", options);
    });

    var startX, startY;
    var isDragging = false;

    canvas.on('mouse:down', function (opt) {
        if (opt.e.altKey !== true) {
            console.log(opt);
            var point = getOriginalPoint(canvas, opt.pointer);
            startX = point.x;
            startY = point.y;
            isDragging = true;
        }
    });

    function addRect(props) {
        var rect = new fabric.Rect(Object.assign(
            {},
            props,
            {
                lockRotation: true,
                centeredRotation: true,
                fill: "#ffff00aa",
            }
        ));

        rect.on('mousedown', function() {
            isDragging = false;
        });

        rect.setControlVisible('mtr', false);

        rect.on('object:moving', function () {
            isDragging = false;
        });

        canvas.add(rect);
        return rect;
    }

    var rectsData = $("#mask-editor").data("rects");
    console.log("Got rects data: ", rectsData);
    rectsData.forEach(function (x) {
        addRect(x);
    });

    canvas.on('mouse:up', function (e) {
        if (!isDragging)  {
            return;
        }

        isDragging = false;
        console.log(e);
        var point = getOriginalPoint(canvas, e.pointer);
        var x = point.x, y = point.y;
        console.log("will draw on: ", startX, startY, x, y);

        var rect = addRect({
            left: startX,
            top: startY,
            height: y - startY,
            width: x - startX,
        });

        canvas.setActiveObject(rect);
        console.log("added rectangle");

    });

    var getRects = function () {
        var ret = [];
        canvas.getObjects().forEach(function (x) {
            if (x.isType("rect")) {
                ret.push({
                    left: x.left,
                    top: x.top,
                    width: x.width * x.scaleX,
                    height: x.height * x.scaleY,
                });
            }
        });
        return ret;
    }

    $("#clear-masks").click(function () {
        getRects().forEach(function (x) {
            console.log("removing ", x);
            canvas.remove(x);
        });
        console.log("clearing masks");
        console.log("and we're done");
        canvas.requestRenderAll();
    });

    $("#mask-editor-form").submit(function () {
        console.log("Submitting form");
        $("input[name='json']").val(JSON.stringify(getRects()));
        console.log("here we go...");
        return true;
    });

    enableZoomOnCanvas(canvas);
    enablePanningOnCanvas(canvas);
}



$(function () {
    if ($("#mask-editor").length > 0) {
        prepareMaskEditor();
    }
});

function enableZoomOnCanvas(canvas) {
    canvas.on('mouse:wheel', function(opt) {
        console.log("on wheel", opt);
        var e = opt.e;
        var delta = e.deltaY * 0.001;
        var vpt = this.viewportTransform;
        var zoom0 = vpt[0];
        var zoom = zoom0 - delta;

        if (zoom > 20) zoom = 20;
        if (zoom < 0.1) zoom = 0.1;


        // The original code used offset{X|Y}
        var canvasPos = getOriginalPoint(canvas, opt.pointer);



        console.log("old values", vpt, canvasPos, zoom, zoom0);

        // We come to do this solution with some simple Matrix algebra
        vpt[4] = (zoom0 - zoom) * canvasPos.x + vpt[4];
        vpt[5] = (zoom0 - zoom) * canvasPos.y + vpt[5];
        vpt[0] = zoom;
        vpt[3] = zoom;
        console.log("before zoom", vpt);
        //canvas.setZoom(zoom);
        //console.log("after zoom", vpt);

        this.requestRenderAll();
        e.preventDefault();


    });
}


function enablePanningOnCanvas(canvas) {
    canvas.on('mouse:down', function(opt) {
        var evt = opt.e;
        if (evt.altKey === true) {
            this.isDragging = true;
            this.selection = false;
            this.lastPosX = evt.clientX;
            this.lastPosY = evt.clientY;
        }
    });
    canvas.on('mouse:move', function(opt) {
        if (this.isDragging) {
            var e = opt.e;
            var vpt = this.viewportTransform;
            vpt[4] += e.clientX - this.lastPosX;
            vpt[5] += e.clientY - this.lastPosY;
            this.requestRenderAll();
            this.lastPosX = e.clientX;
            this.lastPosY = e.clientY;
        }
    });
    canvas.on('mouse:up', function(opt) {
        // on mouse up we want to recalculate new interaction
        // for all objects, so we call setViewportTransform
        this.setViewportTransform(this.viewportTransform);
        this.isDragging = false;
        this.selection = true;
    });

}
