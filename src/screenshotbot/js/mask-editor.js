
$(function () {
    if ($("#mask-editor").length > 0) {

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
        var img = document.getElementById("mask-editor-image");
        var imgInstance = new fabric.Image(img, {
            left: 0,
            top: 0,
            opacity: 0.7,
            selectable: false,
        });
        canvas.add(imgInstance);
        canvas.on('selection:created', function (options) {
            console.log("options: ", options);
        });

        var startX, startY;
        var isDragging = false;
        canvas.on('mouse:down', function (e) {
            console.log(e);
            startX = e.pointer.x;
            startY = e.pointer.y;
            isDragging = true;
        });

        function addRect(props) {
            var rect = new fabric.Rect(Object.assign(
                {},
                props,
                {
                    lockRotation: true,
                    centeredRotation: true,
                    fill: "#ff0000aa",
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
            var x = e.pointer.x, y = e.pointer.y;
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
                    ret.push(x);
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
    }
});
