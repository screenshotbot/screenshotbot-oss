window.addEventListener("chatwoot:ready", function () {
    var body = document.getElementsByTagName("BODY")[0];
    var ds = body.dataset;
    if (ds.userEmail
        // Chatwoot can be buggy, this avoids me polluting existing
        // Chatwoot conversations when debugging things
        && ds.userEmail != "arnold@tdrhq.com") {
        console.log("setting user: " + ds.userEmail);
        window.$chatwoot.setUser(ds.userId, {
            email: ds.userEmail,
            name: ds.userName
        });
    }
});
