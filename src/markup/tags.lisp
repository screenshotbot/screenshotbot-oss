;; Copyright 2019, Modern Interpreters Inc

(uiop:define-package #:markup/tags
    (:use #:cl)
  (:export #:*void-tags*
           #:*standard-names*))
(in-package #:markup/tags)

(defparameter *void-tags*
  (mapcar #'symbol-name
   (list :br
         :hr
         :img
         :input
         :link
         :meta
         :area
         :base
         :col
         :command
         :embed
         :keygen
         :param
         :source
         :track
         :wbr)))


(defparameter *standard-names*
  (mapcar #'symbol-name
          (list :html
                :base
                :head
                :link
                :meta
                :style
                :title
                :body
                :address
                :article
                :aside
                :footer
                :header
                :h1
                :h2
                :h3
                :h4
                :h5
                :h6
                :hgroup
                :main
                :nav
                :section
                :blockquote
                :dd
                :dir
                :div
                :dl
                :dt
                :figcaption
                :figure
                :hr
                :li
                :main
                :ol
                :p
                :pre
                :ul
                :a
                :abbr
                :b
                :bdi
                :bdo
                :br
                :cite
                :code
                :data
                :dfn
                :em
                :i
                :kbd
                :mark
                :q
                :rb
                :rp
                :rt
                :rtc
                :ruby
                :s
                :samp
                :small
                :span
                :strong
                :sub
                :sup
                :time
                :tt
                :u
                :var
                :wbr
                :area
                :audio
                :img
                :map
                :track
                :video
                :applet
                :embed
                :iframe
                :noembed
                :object
                :param
                :picture
                :source
                :canvas
                :noscript
                :script
                :del
                :ins
                :caption
                :col
                :colgroup
                :table
                :tbody
                :td
                :tfoot
                :th
                :thead
                :tr
                :button
                :datalist
                :fieldset
                :form
                :input
                :label
                :legend
                :meter
                :optgroup
                :option
                :output
                :progress
                :select
                :textarea
                :details
                :dialog
                :menu
                :menuitem
                :summary
                :content
                :element
                :shadow
                :slot
                :template
                :acronym
                :applet
                :basefont
                :bgsound
                :big
                :blink
                :center
                :command
                :content
                :dir
                :element
                :font
                :frame
                :frameset
                :image
                :isindex
                :keygen
                :listing
                :marquee
                :menuitem
                :multicol
                :nextid
                :nobr
                :noembed
                :noframes
                :plaintext
                :shadow
                :spacer
                :strike
                :tt
                :xmp

                ;; these are not official HTML5 attributes, so I don't
                ;; know where they come from, but they are definitely
                ;; in the templates I use.
                :svg
                :path
                :rect
                :g
                :polygon
                )))
