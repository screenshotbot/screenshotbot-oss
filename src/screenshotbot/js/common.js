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

    $(".image-comparison-modal").on('show.bs.modal', function () {
        var img = $(".image-comparison-modal-image", this);
        img.attr("src", img.data("src"));
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
