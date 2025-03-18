
/*
 * Code for Remark
 */
$(function () {
    var url = $(".doc-search").find("input").data("search");
    var timeout = null;
    var lastQuery = null;

    function undoSearch($this) {
        $(".actual-content").show();
        $(".remark-outline").css("visibility", "visible");
        $(".search-results").hide();
    }

    function update($this) {
        let query = $this.val();

        if (query === lastQuery) {
            return;
        }
        lastQuery = query;

        if (query === "") {
            undoSearch($this);
            return;
        }

        console.log("querying: ", url, "lastQuery: ", lastQuery, "query: ", query, $this);
        $.ajax({
            url: url,
            method: "POST",
            data: {
                query: query,
            },
            success: (result) => {
                let $results = $(".search-results");
                console.log("results: ", $results);
                $results.html(result)
                $results.show();
                $(".actual-content").hide();
                $(".remark-outline").css("visibility", "hidden");
            }
        });
    }

    let $input = $(".doc-search input");
    $input.on("keyup", () => {
        if (timeout) {
            clearTimeout(timeout);
            timeout = null;
        }
        timeout = setTimeout(() => {
            update($input);
        }, 250);
    });
});
