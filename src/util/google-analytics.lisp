(in-package #:util)
(markup:enable-reader)

(markup:deftag google-analytics (&key tracking (site-speed-sample-rate 100))
  (markup:make-merge-tag
   (list
   <script async="" src=(format nil "https://www.googletagmanager.com/gtag/js?id=~A" tracking)></script>
   <script>
   window.dataLayer = window.dataLayer || [];
   function gtag(){dataLayer.push(arguments);}
   gtag('js', new Date());

   gtag('config', ',(progn tracking)', {'site_speed_sample_rate':,(progn site-speed-sample-rate) });
   </script>)))


(markup:deftag tag-manager-body (&key tracking)
  "Put it early in the <body>"
  <noscript><iframe src= (format nil "https://www.googletagmanager.com/ns.html?id=~a" tracking)
  height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>
  )

(markup:deftag tag-manager-head (&key tracking)
  "Put this early in the <head>"
  <script>(function(w,d,s,l,i){w[l]=w[l]||[];
                   w[l].push({'gtm.start':
                             new Date().getTime(),event:'gtm.js'});
                   var f=d.getElementsByTagName(s)[0],
                   j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';
                   j.async=true;
                   j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;
                   f.parentNode.insertBefore(j,f);
})(window,document,'script','dataLayer',',(progn tracking)');</script>
)
