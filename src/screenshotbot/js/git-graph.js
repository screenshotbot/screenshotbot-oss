/*
 * Code related to the the logic with debugging Git graphs. (See run-page.lisp).
 *
 * If this gets too complicated, remove this from the main default.js,
 * since it's not required.
 */
(function () {

    function setFontWeight(link, fontWeight) {
        var $ref = $($(link).attr("href"));
        $ref.find("td:first-of-type").css("font-weight", fontWeight);
    }

    $("table.git-graph .commit-link").mouseover(
        function () {
            // The href is of the form #commit-id, so we can pass it as a
            // jquery selector.
            setFontWeight(this, "bold");
        },
    ).mouseout(
        function () {
            setFontWeight(this, "normal");
        }
    );
})();
