(defpackage :screenshotbot/sdk/sdk-integration-tests
  (:use #:cl)
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

(defun run-tests ()
  (with-repo
    ;; veryfy we're correctly cloning the repo
    #-mswindows
    (run (list "test" "-e" "gen.sh"))
    (run-gen.sh)
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
