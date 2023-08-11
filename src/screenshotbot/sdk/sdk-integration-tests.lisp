(defpackage :screenshotbot/sdk/sdk-integration-tests
  (:use #:cl)
  (:import-from #:alexandria
                #:assoc-value)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/sdk/sdk-integration-tests)

(uiop:setup-temporary-directory)


(ql:quickload :screenshotbot.sdk/deliver)
(ql:quickload :secure-random)
(ql:quickload :tmpdir)
(ql:quickload :cl-fad)

(defvar *default-cmd-dir* ".")

(defun run (cmd &rest args &key (directory *default-cmd-dir*) &allow-other-keys)
  (let* ((cmd (loop for x in cmd
                    if (pathnamep x)
                      collect (namestring x)
                    else collect x))
         (cmd (loop for x in cmd
                    collect (uiop:escape-shell-command x)))
         (cmd (str:join " " cmd))
         (cmd (format nil "cd ~a && ~a"
                      (uiop:escape-shell-command
                       (cond
                         ((pathnamep directory)
                          (namestring directory))
                         (t
                          directory)))
                      cmd)))
    (log:info "running: ~s" cmd)
    (apply #'uiop:run-program
           (append
            (cond
              ((uiop:os-windows-p)
               (list "cmd" "/c"))
              (t
               (list "bash" "-c")))
            (list
             cmd))
           (append
            (a:remove-from-plist args :directory)
            (list :output t
                  :error-output t)))))

(defvar *sdk* (car (asdf:output-files 'asdf:compile-op
                                       (asdf:find-component :screenshotbot.sdk/deliver
                                                            "deliver-sdk"))))

(when (str:emptyp (uiop:getenv "SCREENSHOTBOT_API_KEY"))
  (error "SCREENSHOTBOT_API_KEY not configured for running integration tests"))

(run (list *sdk* "--help"))

(assert (str:containsp "--static-website" (run (list *sdk* "--help") :output 'string)))

(defmacro with-repo (&body body)
  `(tmpdir:with-tmpdir (dir)
     (run (list "git" "clone" "https://github.com/tdrhq/fast-example" dir))
     (unwind-protect
          (let ((*default-cmd-dir* dir))
            ,@body)

       ;; On Windows, for some reason I'm unable to delete the .git
       ;; directory. Well, not *some* reason. It's just because the
       ;; directories are all marked as read-only. I don't know how
       ;; to clear that up at the moment, so I'm using the nuclear
       ;; option.
       #+(or mswindows win32)
       (run (list "attrib" "-r" (format nil "~a\\.git\\*.*" (namestring dir)) "/s")))))

(defun run-gen.sh ()
  (ensure-directories-exist (format nil "~ascreenshots/" *default-cmd-dir*))
  (run (list "magick" "convert"
             "-size" "360x360" "xc:white" "-fill" "black" "-stroke" "black" "-draw" "@ascii.txt" "-strip" "screenshots/image.png")))


(defun test-crash ()
  (log:info "## TESTING CRASH REPORT TO SENTRY")
  (multiple-value-bind (out err res)
      (run (list *sdk*
                 "--directory"
                 "sdk-integration-test-dummy-dir")
           :ignore-error-status t)
    (log:info "Got error code: ~a" res)
    (assert (not (= res 0)))
    (assert (= 1 res))))

(defun test-large-file ()
  (log:info "Running Large file test")
  (uiop:with-temporary-file (:pathname p :stream s :element-type '(unsigned-byte 8))
    (let ((buff (make-array 1024 :element-type '(unsigned-byte 8)
                            :initial-element 0)))
      (loop for i from 0 below (* 15 1024)
            do
               (write-sequence buff s))
      (finish-output s)
      (let ((res (util/request:http-request
                  (quri:render-uri
                   (quri:merge-uris
                    "/api/upload-test"
                    (or
                     (uiop:getenv "SCREENSHOTBOT_API_HOSTNAME")
                     "https://screenshotbot.io")))
                  :basic-authorization (list
                                        (uiop:getenv "SCREENSHOTBOT_API_KEY")
                                        (uiop:getenv "SCREENSHOTBOT_API_SECRET"))
                  :method :put
                  :content p
                  :want-string t)))
       (assert
        (equal
         15728640 (assoc-value (json:decode-json-from-string res) :response)))))))

(defun test-mark-failed ()
  (log:info "## TESTING --mark-failed")
  (with-repo
   (run (list *sdk*
              "--mark-failed"
              "--channel=foo"))))

(defun test-finalize ()
  (log:info "## TESTING --finalize")
  (with-repo
   (run (list *sdk*
              "--finalize"))))

(defun test-static-website ()
  (log:info "## TESTING --static-website")
  (run (list *sdk*
             "--static-website"
             "src/screenshotbot/sdk/static-example/")
       :directory (uiop:getcwd)))

(defun test-mark-unchanged ()
  (log:info "## TESTING --mark-unchanged-from")
  (with-repo
      (run (list *sdk*
                 "--channel" "bleh"
                 "--mark-unchanged-from" "abcd"))))

(defun run-self-tests ()
  (log:info "Running self tests")
  (run (list *sdk* "--self-test")))

(defun run-tests ()
  (run-self-tests)

  (test-large-file)

  (test-mark-failed)

  (test-finalize)

  (test-static-website)

  (test-mark-unchanged)

  (with-repo
    (test-crash)
    ;; veryfy we're correctly cloning the repo
    (log:info "Finished testing crash")
    #-mswindows
    (run (list "test" "-e" "gen.sh"))
    (run-gen.sh)

    (log:info "Running sdk")
    (run (list *sdk*
               "--directory" "./screenshots"
               "--production=false"))
    #+darwin
    (run (list "arch" "-x86_64"
               *sdk*
               "--directory" "./screenshots"
               "--production=false")))

  (with-repo
      (with-open-file (stream (path:catfile *default-cmd-dir* "ascii.txt")
                              :if-exists :supersede
                              :direction :output)
        (format stream
                "text 15,15 \"Random code: ~a\" " (secure-random:number 10000000000000000000)))

    (run-gen.sh)
    (log:info "final run with random code")
    (run (list *sdk*
               "--directory" "./screenshots"
               "--production=false")))

  #+darwin
  (with-repo
      (with-open-file (stream (path:catfile *default-cmd-dir* "ascii.txt") :if-exists :supersede
                                          :direction :output)
        (format stream
                "text 15,15 \"Random code: ~a\" " (secure-random:number 10000000000000000000)))
    (run-gen.sh)
    (run (list "arch" "-x86_64" *sdk*
               "--directory" "./screenshots"
               "--production=false"))))

(defun run-tests-with-tmpdir ()
  (tmpdir:with-tmpdir (dir)
    (let ((*default-cmd-dir* dir))
      (run-tests))))

(run-tests-with-tmpdir)
