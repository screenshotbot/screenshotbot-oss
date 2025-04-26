jQuery.timeago.settings.allowFuture = true;

// Stop JS animations if we're using Selenium
if ($("link[href='/assets/css/selenium.css']").length) {
    $.fx.off = true;
}

$(function () {

    var stripeKey = $("#stripe-key").val();

    if (stripeKey === undefined) {
        return;
    }

    console.log("dfdfdfdf");

    // Create a Stripe client.
    var stripe = Stripe(stripeKey);

    // Create an instance of Elements.
    var elements = stripe.elements();

    // Custom styling can be passed to options when creating an Element.
    // (Note that this demo uses a wider set of styles than the guide below.)
    var style = {
        base: {
            color: '#32325d',
            fontFamily: '"Helvetica Neue", Helvetica, sans-serif',
            fontSmoothing: 'antialiased',
            fontSize: '16px',
            '::placeholder': {
                color: '#aab7c4'
            }
        },
        invalid: {
            color: '#fa755a',
            iconColor: '#fa755a'
        }
    };

    // Create an instance of the card Element.
    var card = elements.create('card', {style: style});

    // Add an instance of the card Element into the `card-element` <div>.
    card.mount('#card-element');

    // Handle real-time validation errors from the card Element.
    card.addEventListener('change', function(event) {
        var displayError = document.getElementById('card-errors');
        if (event.error) {
            displayError.textContent = event.error.message;
        } else {
            displayError.textContent = '';
        }
    });
    // Handle form submission.
    var form = document.getElementById('payment-form');
    form.addEventListener('submit', function(event) {
        event.preventDefault();

        stripe.createToken(card).then(function(result) {
            if (result.error) {
                // Inform the user if there was an error.
                var errorElement = document.getElementById('card-errors');
                errorElement.textContent = result.error.message;
            } else {
                // Send the token to your server.
                stripeTokenHandler(result.token);
            }
        });
    });

    // Submit the form with the token ID.
    function stripeTokenHandler(token) {
        // Insert the token ID into the form so it gets submitted to the server
        makeHiddenField("stripe-token", token.id);
        makeHiddenField("card", JSON.stringify(token.card));
        // Submit the form
        form.submit();
    }

    function makeHiddenField(name, value) {
        var form = document.getElementById('payment-form');
        var hiddenInput = document.createElement('input');
        hiddenInput.setAttribute('type', 'hidden');
        hiddenInput.setAttribute('name', name);
        hiddenInput.setAttribute('value', value);
        form.appendChild(hiddenInput);

    }
});


$("time.timeago").timeago();

function getSelectedRuns() {
    var ret = [];
    $(".recent-run-item").map(function () {
        if ($(this).prop("checked")) {
            var id = $(this).data("model-id");
            ret.push(id);
        }
    });
    return ret;
}

function swAlert(x) {
    alert(x);
}

$("#delete-runs").click(function (e) {
    console.log("#delete-runs clicked");
    var runs = getSelectedRuns();
    var hadPromotedRuns = false;
    function deleteNextRun() {
        if (runs.length == 0) {
            if (hadPromotedRuns) {
                swAlert("Some runs could not be deleted because they were previously promoted");
            }
            location.reload();
            return;
        }

        var id = runs.pop();
        console.log("deleting: " + id);
        $.ajax({
            url: "/runs/" + id,
            method: "DELETE",
            success: function (data) {
                if (data.wasPromoted) {
                    hadPromotedRuns = true;
                }
                deleteNextRun();
            },
            error: function () {
                swAlert("Could not delete run, maybe refresh and retry?");
            }
        });
    }
    deleteNextRun();
    e.preventDefault()
});

$("#compare-runs").click(function () {
    var runs = getSelectedRuns();
    if (runs.length > 2) {
        swAlert("Select only two runs to compare.");
        return;
    }

    if (runs.length != 2) {
        swAlert("Select two runs to compare");
        return;
    }

    var min = runs[1];
    var max = runs[0];

    location.href = "/runs/" + max + "/compare/" + min;
});

$(function () {
    $(".github-test-connection").click(function () {
        var button = this;
        $(".spinner", this).removeClass("d-none");
        var oldtext = $(".text", this).text();
        $(".text", this).text("Checking...");
        function reset () {
            $(".spinner", button).addClass("d-none");
            $(".text", button).text(oldtext);
        }
        $.ajax({
            dataType: "json",
            method: "POST",
            data: {
                "github-link": $("#github-link").val(),
            },
            url: "/github/test-integration",
            success: function (data) {
                var feedback =
                    $(".invalid-feedback", $(button).parent());
                var input =
                    $("#github-link");


                if (data["result"]) {
                    $(input).addClass("is-valid");
                    $(input).removeClass("is-invalid");
                    $(".valid-feedback", $(button).parent())
                        .text("We are able to connect to this repository");
                } else {
                    $(input).addClass("is-invalid");
                    $(input).removeClass("is-valid");
                    $(feedback).text("Unable to connect to this repository");
                }
                reset();
            },
            error: function() {
                reset();
                alert("something went wrong, try again in a minute");
            }
        });
        return false;
    });
});

function setupNoticeDismiss() {
    $(".user-notice-dismiss").click(function () {
        var noticeId = $(this).data("notice-id");
        console.log("dismissing notification");
        $.ajax({
            method: "POST",
            url: "/notice/dismiss",
            data: {
                "notice-id": noticeId
            },
            success: function (data) {
                $("#user-notice-list").replaceWith(data);
                setupNoticeDismiss();
            },
            error: function () {
                console.log("something went wrong. Please contact support@");
            }
        });
    });
}

setupNoticeDismiss();

$(".async-fetch").map(function (idx, elm) {
    console.log("Element is", elm);
    function onError() {
        $(elm).replaceWith("<h3>Error while loading</h3>");
    }

    function tryNext() {
        var status = $(elm).data("check-nibble");
        console.log("Fetching state from " + status);
        $.ajax({
            method: "GET",
            url: status,
            dataType: "json",
            success: function (data) {
                console.log(data);
                if (data["state"] == "processing") {
                    setTimeout(tryNext, 1000);
                } else if (data["state"] == "done") {
                    var newData = $(data["data"]);
                    $(elm).replaceWith(newData);
                    callLiveOnAttach(newData);
                } else {
                    onError();
                }
            },
            error: onError,
        });
    }

    tryNext();
});


setupLiveOnAttach(".load-more-button", function () {
    var isInfinite = $(this).data("infinite");

    var disabled = false;

    
    $(this).click(function (e) {
        var button = $(this);
        var link = $(button).data("load-more");
        console.log("Fetching next page");

        function setDisabled(val) {
            $(button).prop("disabled", val);
            disabled = val;

            setTimeout(() => {
                // We don't want the spinner to show up if it's less
                // than a certain amount of time since it's distracting.
                $(button).addClass("spinner");
            }, 250);
        }

        setDisabled(true);

        $.ajax({
            method: "GET",
            url: link,
            success: function (data) {
                console.log("Got next page");
                var div = $(data);
                var container = $(button).closest(".load-more-container");
                var children = div.children();
                children.appendTo(container);
                callLiveOnAttach(children);
                var bb = button.closest(".baguetteBox");
                button.parent().remove();
                if (bb.length > 0) {
                    baguetteBox.destroy("#" + bb.attr("id"));
                    prepareBaguetteBox(bb);
                }
            },
            error: function(data) {
                setDisabled(false);
                alert("Something went wrong, please try refreshing the page");
            }
        });

        e.preventDefault();
    });

    if (isInfinite) {
        console.log("Setting up infinite scroll");
        var button = $(this);
        var called = false;
        var callback = () => {
            if (disabled) {
                return;
            }

            const scrollTop = window.scrollY; // How much we've scrolled from the top
            const windowHeight = window.innerHeight; // The height of the visible window
            const documentHeight = document.documentElement.scrollHeight; // Total height of the page
            
            const scrollPercentage = (scrollTop + windowHeight) / documentHeight;
            
            if (scrollPercentage >= 0.8) {
                console.log("Hit target for infinite scroll ", this);
                button.click();
            }
        }

        $(window).on("scroll", callback);
    }
});


$(function () {
    $(".codemirror-textarea").map(function () {
        var codeMirror = CodeMirror.fromTextArea(
            this, {
                value: "foobar:",
                mode: "text/x-yaml",
                lineNumbers: true,
            });
        codeMirror.setSize("100%", "100%");
    });
});


$(".new-compare").each(function () {
    Split([$(this).find(".image-list").get(0), $(this).find(".image-details").get(0)]);
});


var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="tooltip"]'))
var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
  return new Tooltip(tooltipTriggerEl)
})

$(function () {
    var popoverTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="popover"]'))
    var popoverList = popoverTriggerList.map(function (popoverTriggerEl) {
        return new Popover(popoverTriggerEl)
    })
});


$(function () {
    if (window.location.pathname == "/") {
        if ($("body").hasClass("dashboard")) {
            // To avoid redirection when you first load screenshotbot,
            // we'll load /, and then replace the URL.
            history.replaceState(null, null, "/runs");
        }
    }
});
