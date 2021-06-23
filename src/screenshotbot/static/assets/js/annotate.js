window.addEventListener('DOMContentLoaded', function () {
    var cvs = document.querySelector("#annotation-editor");
    if (cvs !== null) {
        console.log("found Annotation canvas");
        var img = document.querySelector("#annotation-editor-img");
        console.log("using image: ", img);
        cvs.height = img.height;
        cvs.width = img.width;
        cvs.style.width = img.width / 10 + "rem";
        cvs.style.height = img.height / 10 + "rem";
        var canvas = new fabric.Canvas(cvs);
        var imgInstance = new fabric.Image(img, {
            left: 0,
            top: 0,
            selectable: false,
            opacity: 1.0,
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

        function updateImageData() {
            canvas.requestRenderAll();
            cvs.closest("form").querySelector("input[name='image-data']").value =
                canvas.toDataURL();
        }

        function addRect(props) {
            var rect = new fabric.Rect(Object.assign(
                {},
                props,
                {
                    lockRotation: true,
                    centeredRotation: true,
                    fill: "#ff000000",
                    stroke: "#ff0000ff",
                    strokeWidth: 5,
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

            updateImageData();

            return rect;
        }


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
        console.log("all done");
        canvas.requestRenderAll();

    }
});
