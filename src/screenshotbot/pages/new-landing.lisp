;; -*- encoding: utf-8 -*-
(in-package :screenshotbot)

(markup:enable-reader)

(deftag new-nav-link (children &key href)
  <nav-link href=href >,@ (progn children)</nav-link>)

(deftag new-landing-nav-bar (&key transparent)
  <div class= "container-fluid">
    <nav class= "navbar headroom navbar-expand-lg fixed-top" >
      <div class= "container-fluid">
        <a class= "navbar-brand" href= "/">
          <img src= "/assets/images/logo-dark.png" />
        </a>
        <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation"
                data-bs-parent= "#navbarSupportedContent" >
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class= "collapse navbar-collapse" id= "navbarSupportedContent" >
          <ul class= "navbar-nav me-auto mb-2 mb-lg-0" >
            <li class= "nav-item" >
              <new-nav-link href= "/pricing">Pricing</new-nav-link>
            </li>
            <li class= "nav-item" >
              <new-nav-link href= "/integrations">Integrations</new-nav-link>
            </li>
            <li class= "nav-item" >
              <new-nav-link href= "/about">About</new-nav-link>
            </li>
          </ul>

          <div>
            <ul class= "navbar-nav me-auto mb-2 mb-lg-0">
              <li class= "nav-item">
                <new-nav-link href= "/documentation">Docs</new-nav-link>
              </li>
              <li class= "nav-item">
                <new-nav-link href= "/login">Sign In</new-nav-link>
              </li>

              <li class= "nav-item">
                <a href= "/signup" class= "nav-link get-started-link ">
                  Get Started
                </a>
              </li>
            </ul>
          </div>

        </div> <!-- navbar-collapse -->

      </div>
    </nav>
  </div>)

(deftag new-sep ()
    <img src= "/assets/images/new-landing/separator.svg" class= "new-sep" />)

(deftag new-landing-template (children
                              &key transparent
                              (nav-bar t)
                              (footer t)
                              (body-class "")
                              (style "/assets/css/new-landing.css")
                              scripts
                              dashboard-js
                              (stripe t))
  <html lang= "en">
    <landing-head style=style />
    <body class=body-class >
      ,(when nav-bar
         <new-landing-nav-bar transparent=transparent   />)
      ,@children
         <script src= "/assets/js/new-landing.js" />
    </body>
  </html>)

(defhandler (nil :uri "/new-landing") ()
  (hex:safe-redirect "/"))

(defun new-landing-page ()
    <new-landing-template
     body-class= "landing-page" >
    <section class= "container-fluid new-hero">
      <h1>Visual Testing Made Simple</h1>
      <h3>Manual testing is holding you back. We help you monitor your application for UI changes automatically.</h3>
      <!-- <h3>Monitor your application for UI changes, while avoiding tedious manual testing.</h3> -->

      <div  class= "hero-get-started-wrapper" >
        <a href= "/signup" class= "btn btn-primary hero-get-started" >Get Started for Free</a>
      </div>

      <div>
        <img class= "hero-image" src= "/assets/images/new-landing/landing-hero.png" />
      </div>
      <new-sep />
    </section>

  <new-integrations-summary />
     <work-smarter-section />

    <new-landing-details-section />

    <new-landing-transparent-pricing />

  <new-landing-docs-section />
  <new-landing-ready-to-get-started />
  <new-footer />

  </new-landing-template>)

(defhandler (nil :uri "/home") ()
  (new-landing-page))

(deftag new-integrations-summary ()
  (flet ((img (x)
             <li>
               <img src= (format nil "/assets/images/integrations-summary/~a.png" x) />
             </li>))
    <section class= "integrations-summary" >
    <div class= "container-fluid">
      <ul>
        ,(img "slack")
        ,(img "github")
        ,(img "jira")
        ,(img "asana")
        ,(img "firebase")
        ,(img "trello")
        ,(img "circleci")
        ,(img "buildkite")
        ,(img "travis")
        ,(img "bitbucket")
        ,(img "gitlab")
      </ul>
      <p>
        Mobile-first automated screenshot testing, that integrates with all your tools
      </p>
    </div>
  </section>))

(deftag work-smarter-section ()
  <section class= "work-smarter" id= "work-smarter" >
    <div class= "container-fluid">
      <h1>Work Smarter, Not Harder</h1>
      <h3>Here's how it works in four simple steps</h3>

      <div class= "row" >
        <div class= "col-6 col-md-3" >
          <div class= "step-box yellow" ><span>1</span><i /></div>
          <h4>Build and test your code on your favorite platform</h4>
        </div>
        <div class= "col-6 col-md-3" >
          <div class= "step-box green" ><span>2</span><i /></div>
          <h4>Screenshotbot tracks UI changes in screenshots from your tests</h4>
        </div>
        <div class= "col-6 col-md-3" >
          <div class= "step-box pink" ><span>3</span><i /></div>
          <h4>Get notified of UI changes and regressions</h4>
        </div>
        <div class= "col-6 col-md-3" >
          <div class= "step-box blue" ><span>4</span><i /></div>
          <h4>Keep track of project progress over time</h4>
        </div>
      </div>
    </div>
  </section>)

(deftag new-landing-details-section ()
  <section class= "details-section">
    <div class= "container-fluid">
      <div class= "row g-0 row-1" >
        <div class= "col-lg-6 image-box">
          <div class= "img-container">
            <img class= "landing-details-img" src= "/assets/images/new-landing/yellow-big-image.png" />
          </div>
        </div>

        <div class= "col-lg-6 text-area">
          <h1 >Build with Confidence</h1>
          <p>Mobile or desktop, we've got you covered. We have seamless integrations for Android, iOS and Selenium. You can use the programming language and CI of your choice. </p>

          <a href= "/documentation?s=y" class= "btn learn-more" id= "build-with-confidence" >Learn More</a>
        </div>
      </div>

      <div class= "row g-0 row-1 green" >
        <div class= "col-lg-6 text-area">
          <h1 >Review and Rewind</h1>
          <p>
            Our platform provides sophisticated tools to visualize and understand regressions. We also store the entire history of your screenshots so you can easily bisect old regressions.
          </p>

          <a href= "/documentation?s=2" class= "btn learn-more" id= "review-and-rewind" >See Our Features</a>
        </div>

        <div class= "col-lg-6 image-box">
          <div class= "img-container">
            <img class= "landing-details-img" src= "/assets/images/new-landing/green-big-image.png" />
          </div>
        </div>
      </div>

      <div class= "row g-0 row-1 red" >
        <div class= "col-lg-6 image-box">
          <div class= "img-container">
            <img class= "landing-details-img" src= "/assets/images/new-landing/pink-big-image.png" />
          </div>
        </div>

        <div class= "col-lg-6 text-area">
          <h1 >Get Notified on Changes Before You Merge</h1>
          <p>
            Get notified on GitHub, GitLab, Phabricator and BitBucket. We’ll update your pull request/merge request/diff with a report of screenshots that changed.
          </p>

          <a href= "/documentation?s=3" class= "btn learn-more" id= "before-you-merge" >Learn More</a>
        </div>
      </div>

      <div class= "row g-0 row-1 blue" >
        <div class= "col-lg-6 text-area">
          <h1 >Keep Your Team in the Loop</h1>
          <p>
            Screenshotbot can create tasks on Jira, Trello, Asana, or any project management tool of your choice. Give QA, designers, project managers and other stakeholders the chance to provide early feedback.
          </p>

          <a href= "/documentation?s=4" class= "btn learn-more" id= "after-you-merge" >Learn More</a>
        </div>

        <div class= "col-lg-6 image-box">
          <div class= "img-container">
            <div class= "landing-details-img">
              <img src= "/assets/images/new-landing/blue-big-image.png" />
              <span class= "badge">1</span>
            </div>
          </div>
        </div>
      </div>

    </div>
  </section>)

(deftag new-landing-transparent-pricing ()
  <section class= "transparent-pricing" id= "transparent-pricing" >
    <div class= "container-fluid">
      <h2>Transparent Pricing</h2>
      <p class= "subtitle" >We like to keep things simple. Our straightforward pricing has no hidden costs.
        And we’re always free for open source projects.</p>
      <new-landing-pricing-table />
  </div>
  </section>)

(deftag new-landing-pricing-table ()
        <div class= "row g-10 gy-4 pricing-row">
        <div class= "col-md-4">
          <div class= "pricing-card" >
            <h4>Basic</h4>
            <h5>$0 USD</h5>
            <div class= "price-desc" ><span>Free forever</span></div>

            <h6>Feature:</h6>
            <ul>
              <li>100 screenshots per day</li>
              <li>Up to 3 users</li>
              <li>History up to 3 months</li>
            </ul>

            <a href= (make-url "/signup" :plan :personal) class= "btn btn-cta">Get Started</a>
          </div>
        </div>

        <div class= "col-md-4">
          <div class= "pricing-card">
            <H4>Standard</H4>
            <H5>$95 USD</H5>
            <div class= "price-desc" ><span>Per month</span></div>

            <h6>In Basic plus:</h6>
            <ul>
              <li>Unlimited screenshots</li>
              <li>Unlimited users</li>
              <li>History up to 1 year</li>
            </ul>

            <a href= (make-url 'pro-plan-signup) class= "btn btn-cta">Start 30-Day Trial</a>
          </div>
        </div>

        <div class= "col-md-4">
          <div class= "pricing-card">
            <h4>Enterprise</h4>
            <h5>Custom</h5>
            <div class= "price-desc" ><span>Build your own plan</span></div>

            <h6>Premium:</h6>
            <ul>
              <li>Dedicated support</li>
              <li>Unlimited history</li>
              <li>Highest security guarantees</li>
            </ul>

            <a href= "mailto:sales@screenshotbot.io" class= "btn btn-primary btn-cta enterprise">Schedule Demo</a>
          </div>
        </div>
        </div>)

(deftag new-landing-docs-section ()
  <section class= "docs-section" id= "docs-section" >
    <div class= "container-md">
    <div class= "g-0 black-box" >
      <div class= "col-image" >
        <img class= "blog-image" src= "/assets/images/new-landing/blog-image.png" />
      </div>

      <div class= "content">
        <h4>Documentation</h4>
        <h2><a href= "/documentation/why-screenshot-tests" class= "hidden-link">Why Screenshot Tests?</a></h2>

        <p>
          Some engineers may be against the idea of screenshot tests and might argue that one should write unit tests instead.
        </p>

        <div class= "bottom-bar">
          <span class= "time-overview">
            2 min
          </span>

          <a href= "/documentation/why-screenshot-tests" class= "read-more-link">Read More</a>
        </div>
      </div>
    </div>

    <div class= "white-boxes">
      <div class= "white-box">
        <h3><a href="/documentation/github" class= "hidden-link">GitHub Integration</a></h3>

        <p>
          Screenshotbot integrates seamlessly into your pull request flow with GitHub. This document explains how you can set it up in few easy steps.
        </p>

        <div class= "bottom-bar">
          <span class="time-overview">
            2 min
          </span>
          <a href= "/documentation/github" class= "read-more-link">Read More</a>
        </div>
      </div>

      <div class= "white-box">
        <h3><a class= "hidden-link" href= "/blog/what-is-software-testing">What is Software Testing</a></h3>

        <p>
          What is software testing and different methods of testing
        </p>

        <div class= "bottom-bar">
          <span class="time-overview">
            2 min
          </span>
          <a href= "/blog/what-is-software-testing" class= "read-more-link">Read More</a>
        </div>
      </div>
    </div>
    </div>


  </section>)

(deftag new-landing-ready-to-get-started ()

  <section id= "ready-to-get-started" >
    <div class= "container container-with-padding">
      <div class= "row align-items-center">
        <div class= "col-md-6">
          <h2>Ready to get started?</h2>
          <p>Sign up or contact us.</p>
        </div>
        <div class= "col-md-6" >
          <div class= "row g-2 align-items-center">
            <div class= "col-lg-7 ">
              <a href= "/request-demo" class= "btn btn-primary" >Request a Demo</a>
            </div>
            <div class= "col-lg-5 ">
              <a href= "/signup" class= "btn btn-secondary" >Start for Free</a>
            </div>
          </div>
        </div>
      </div>
    </div>
  </section>)

(deftag new-footer ()
  <footer id= "footer" >
    <div class= "container-fluid container-with-padding pb-6">
      <div class= "footer-div g-2">
          <div class= "info-box">
            <img src= "/assets/images/new-landing/logo-footer.png" class= "logo" />
            <h4>Build Pixel Perfet Apps with friendly and reliable screenshot testing</h4>

            <ul class= "social">
              <li ><a href= "https://www.facebook.com/getScreenshotbot/" target= "_blank"><img src= "/assets/images/new-landing/facebook-logo.svg" /></a></li>
              <li class= "twitter" ><a href= "https://twitter.com/screenshotb0t/" target= "_blank" ><img src= "/assets/images/new-landing/twitter-logo.svg" /></a> </li>
              <li class= "github"><a href= "https://github.com/moderninterpreters" target= "_blank" ><img src= "/assets/images/new-landing/github-logo.svg" /></a></li>
              <li class= "linkedin"><a href= "https://www.linkedin.com/showcase/70565060/" target= "_blank" ><img src= "/assets/images/new-landing/linkedin-logo.svg" /></a></li>
            </ul>

            <h5>© 2021 Modern Interpreters Inc.</h5>
          </div>

        <div class= "link-list-cn">
      <div class= "link-list" >
        <h5>Product</h5>
        <ul>
          <li><a href= "/integrations">Integrations</a></li>
          <li><a href= "/pricing">Pricing</a></li>
          <li><a href= "/faqs">FAQs</a></li>
        </ul>
      </div>

      <div class= "link-list">
        <h5>Developers</h5>
        <ul>
          <li><a href= "/documentations">Documentation</a></li>
        </ul>
      </div>

      <div class= "link-list" >
        <h5>Community</h5>
        <ul>
          <li><a href= "#">Discord</a></li>
        </ul>
      </div>

      <div class= "link-list" >
        <h5>Company</h5>
        <ul>
          <li><a href= "/about">About</a></li>
          <li><a href= "/blog">Blog</a></li>
          <li><a href= "/support">Support</a></li>
        </ul>
      </div>

      <div class= "link-list" >
        <h5>Legal</h5>
        <ul>
          <li><a href= "/terms">Terms and Conditions</a></li>
          <li><a href= "/privacy">Privacy Policy</a></li>
        </ul>
      </div>
      </div>


    </div>
    </div>
  </footer>)
