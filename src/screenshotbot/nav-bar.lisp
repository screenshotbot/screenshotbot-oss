(in-package :screenshotbot)

(markup:enable-reader)

(deftag nav-link (children &key href (class ""))
  (let ((active (str:starts-with-p href (hunchentoot:script-name hunchentoot:*request*))))
    <a href=href class= (format nil "nav-link ~a~a" class
                                (if active " active" ""))>
                                ,@children
                                </a>))

(deftag landing-nav-bar (&key transparent (style :dark))
  <nav class= (format nil "navbar headroom navbar-expand-lg py-lg-3 ~a ~a"
               (ecase style
       (:light "navbar-light")
       (:dark "navbar-dark"))
                      (if transparent "" "bg-primary")) >
            <div class="container">

                <!-- logo -->
                <a href="/" class="navbar-brand mr-lg-2">
                  <img src=
                       (format nil "/assets/images/logo-~a.png" (ecase style
                       (:light "dark")
                       (:dark "light"))) class= "navbar-brand-logo" />
                </a>

                <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarNavDropdown"
                    aria-controls="navbarNavDropdown" aria-expanded="false" aria-label="Toggle navigation">
                    <i class="mdi mdi-menu"></i>
                </button>

                <!-- menus -->
                <div class="collapse navbar-collapse" id="navbarNavDropdown">

                    <!-- left menu -->
                    <ul class="navbar-nav me-auto align-items-center">
                        <li class="nav-item mx-lg-1">
                            <nav-link class="nav-link" href="/pricing">Pricing</nav-link>
                        </li>

                        <li class="nav-item mx-lg-1">
                            <nav-link class="nav-link" href="/faqs">FAQs</nav-link>
                        </li>

                        <li class= "nav-item mx-lg-1">
                          <nav-link class= "nav-link" href= "/documentation/">Documentation</nav-link>
                        </li>

                        <li class="nav-item mx-lg-1">
                          <nav-link class="nav-link" href= "/about"
                                    >About Us</nav-link>
                        </li>
                    </ul>

                    <!-- right menu -->
                    <ul class="navbar-nav ms-auto align-items-center">
                        <li class="nav-item mx-lg-1">
                            <a class="nav-link" href="/login">Sign in</a>
                        </li>
                        <li class="nav-item mr-0">
                          <a href= "/signup"  class= (format nil
                                                      "btn btn-sm ~a btn-rounded d-none d-lg-inline-flex sign-up-button"
                                                      (ecase style
                             (:light "btn-primary")
                             (:dark "btn-light"))) >
                                Sign up
                            </a>
                        </li>
                    </ul>

                </div>
            </div>
        </nav>)
