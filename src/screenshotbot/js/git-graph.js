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

    function highlightBranch(commit) {
        var seen = new Set();
        var queue = [];
        var pos = 0;
        queue.push("#" + commit);

        while (pos < queue.length) {
            var commit = queue[pos++];

            if (seen.has(commit)) {
                continue;
            }

            seen.add(commit);

            var $td = $(commit);
            $td.addClass("highlighted");
            $td.find(".commit-link").each(function () {
                queue.push($(this).attr("href"));
            });
        }
    }

    $("table.git-graph .highlight-branch").click(function () {
        $("table.git-graph tr").removeClass("highlighted");
        highlightBranch($(this).data("commit"));
    });
})();
