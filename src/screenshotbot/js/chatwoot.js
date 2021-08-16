window.addEventListener("chatwoot:ready", function () {
    var body = document.getElementsByTagName("BODY")[0];
    var ds = body.dataset;
    if (ds.userEmail) {
        console.log("setting user: " + ds.userEmail);
        window.$chatwoot.setUser(ds.userId, {
            email: ds.userEmail,
            name: ds.userName
        });
    }
});
