
function runBrowserTests() {
    function log(msg) {
        var $span = $("<div></div>");
        $span.append(msg);
        $("#results").append($span);
    }

    log("Starting up");

    var counter = 1;
    const timeout = setInterval(function () {
        counter ++;
        log("Timeout log for " + (counter * 100) + "ms mark");
    }, 100);

    var startTime = new Date();

    function getElapsedTime() {
        return ( (new Date()).getTime() - startTime.getTime())/1000;
    }

    function logElapsedTime() {
        log("Took " + getElapsedTime() + " seconds");
        clearInterval(timeout);
    }

    var domain = $("body").data("domain");
    log("Fetching " + domain);

    $.ajax({
        url: domain,
        method: "GET",
        success: function (){
            log("ERROR: We were able to fetch " + domain);
            logElapsedTime();

            secondTest();
        },
        error: function (jqxhr, textStatus){
            log("SUCCESS: " + domain + " was inaccessible because of: " + textStatus);
            logElapsedTime();
            if (getElapsedTime() > 5) {
                log("ERROR: Took a while to result in an failed ajax request");
            }
        }

    });
}

runBrowserTests();
