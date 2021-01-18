(in-package :bknr.utils)

(define-constant +itoa64+
  "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defun itoa64 (int num)
  (loop for i below num
	collect (char +itoa64+ (ldb (byte 6 (* 6 i)) int))))

(defun to64 (md5)
  (let ((res (mapcan #'(lambda (indices)
			  (itoa64 (loop with ret = 0
					for i from 0 by 8
					for idx in indices
					do (setf (ldb (byte 8 i) ret) (elt md5 idx))
					finally (return ret)) 4))
		      '((12 6 0)
			(13 7 1)
			(14 8 2)
			(15 9 3)
			(5 10 4)))))
    (coerce (nconc res (itoa64 (elt md5 11) 2)) 'string)))

(defun extract-salt (saltpw)
  (coerce (loop for ch across (subseq saltpw 3)
		until (eql ch #\$)
		collect ch) 'string))

(defun crypt-md5 (pw salt)
  (let ((pw (if (typep pw 'simple-array)
                pw
                (make-array (length pw)
                            :element-type (array-element-type pw)
                            :initial-contents pw))))
    (when (string-equal (subseq salt 0 3) "$1$")
      (setf salt (extract-salt salt)))
  
    (let ((s (make-md5-state))
          (s2 (make-md5-state)))

      (update-md5-state s pw)
      (update-md5-state s "$1$")
      (update-md5-state s salt)

      (update-md5-state s2 pw)
      (update-md5-state s2 salt)
      (update-md5-state s2 pw)

      (let ((str (finalize-md5-state s2)))
        (update-md5-state s (subseq str 0 (min (length pw) 16)))
        (setf (elt str 0) 0)
        (loop for i = (length pw) then (ash i -1)
           until (<= i 0)
           do (update-md5-state s (if (oddp i) str pw) :end 1))
        (let ((seq (finalize-md5-state s)))

          (dotimes (i 1000)
            (let ((s3 (make-md5-state)))
              (update-md5-state s3 (if (oddp i) pw seq))
              (unless (= (mod i 3) 0)
                (update-md5-state s3 salt))
              (unless (= (mod i 7) 0)
                (update-md5-state s3 pw))
              (if (oddp i)
                  (update-md5-state s3 seq)
                  (update-md5-state s3 pw))
              (setf seq (finalize-md5-state s3))))
          (concatenate 'string "$1$" salt "$" (to64 seq)))))))

(defun verify-md5-password (password saltpw)
  (unless (string-equal (subseq saltpw 0 3) "$1$")
    (error "not a md5 password ~a" saltpw))
  (let ((salt (extract-salt saltpw)))
    (string-equal (crypt-md5 (coerce password 'simple-string) salt) saltpw)))

;; 0 6 12 (4)
;; 1 7 13 (4)
;; 2 8 14 (4)
;; 3 9 15 (4)
;; 4 10 5 (4)
;; 11     (2)
