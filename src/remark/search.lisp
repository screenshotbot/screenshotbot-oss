(defpackage :remark/search
  (:use #:cl)
  (:import-from #:remark/nodes
                #:page-text
                #:find-node
                #:section-children
                #:section
                #:page)
  (:import-from #:util/events
                #:push-event)
  (:import-from #:util/throttler
                #:throttle!
                #:ip-throttler)
  (:export
   #:search-remarks))
(in-package :remark/search)

(defparameter *throttler* (make-instance 'ip-throttler
                                         :tokens 3600
                                         :period 3600))

(defmethod search-remarks ((page page) query)
  (throttle! *throttler*)
  (push-event :search-docs :query query)
  (when (str:containsp (str:downcase query) (page-text page))
    ;;(log:info "Page text is: ~a" (page-text page))
    (list page)))

(defmethod search-remarks ((section section) query)
  (loop for child in (section-children section)
        appending (search-remarks (find-node child) query)))
