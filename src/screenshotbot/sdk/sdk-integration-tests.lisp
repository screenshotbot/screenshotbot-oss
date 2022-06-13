(defpackage :screenshotbot/sdk/sdk-integration-tests
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/sdk/sdk-integration-tests)

(ql:quickload :screenshotbot.sdk.deliver)
(ql:quickload :secure-random)

(defun run (cmd &rest args)
  (apply #'uiop:run-program
           (loop for x in cmd
                 if (pathnamep x)
                   collect (namestring x)
                 else collect x)
           (append
            args
            (list :output t
                  :error-output t))))

(defvar *sdk* (car (asdf:output-files 'asdf:compile-op
                                       (asdf:find-component :screenshotbot.sdk.deliver
                                                            "deliver-sdk"))))

(when (str:emptyp (uiop:getenv "SCREENSHOTBOT_API_KEY"))
  (error "SCREENSHOTBOT_API_KEY not configured for running integration tests"))

(run (list *sdk* "--help"))

(assert (str:containsp "--static-website" (run (list *sdk* "--help") :output 'string)))

(defmacro with-repo (&body body)
  `(tmpdir:with-tmpdir (dir)
     (run (list "git" "clone" "https://github.com/tdrhq/fast-example" dir))
     (let ((*original-dir* (uiop:getcwd)))
       (unwind-protect
            (progn
              (uiop:chdir dir)
              ,@body)
         (uiop:chdir *original-dir*)))))


(with-repo
    ;; veryfy we're correctly cloning the repo
    (assert (path:-e "gen.sh"))
  (run (list "./gen.sh"))
  (run (list *sdk*
             "--directory" "./screenshots"
             "--production=false"))
  #+darwin
  (run (list "arch" "-x86_64"
             *sdk*
             "--directory" "./screenshots"
             "--production=false")))

(with-repo
    (with-open-file (stream "ascii.txt" :if-exists :supersede
                            :direction :output)
      (format stream
              "text 15,15 \"Random code: ~a\" " (secure-random:number 10000000000000000000)))
  (run (list "./gen.sh"))
  (run (list *sdk*
             "--directory" "./screenshots"
             "--production=false")))

#+darwin
(with-repo
    (with-open-file (stream "ascii.txt" :if-exists :supersede
                            :direction :output)
      (format stream
              "text 15,15 \"Random code: ~a\" " (secure-random:number 10000000000000000000)))
  (run (list "./gen.sh"))
  (run (list "arch" "-x86_64" *sdk*
             "--directory" "./screenshots"
             "--production=false")))
