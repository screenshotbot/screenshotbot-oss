;; -*- Lisp -*-

;; make-fdf-file.lisp

;; Funktion zum Erstellen von FDF-Dateien.  Diese können mit Hilfe von
;; pdftk verwendet werden, um PDF-Formulare auszufuellen.  Das
;; FDF-Format ist dabei ein Unterformat von Adobe PDF und wird in der
;; PDF-Spezifikation beschrieben.

(in-package :bknr.utils)

(enable-interpol-syntax)

(defun pdf-quote-string (string)
  (regex-replace-all #?r"([\(\)\\])" string #?r"\\\1"))

(defun make-fdf-file (file-name &rest keys-and-values)
  (with-open-file (stream file-name :direction :output :if-does-not-exist :create :if-exists :supersede :external-format :latin-1)
    (format stream "%FDF-1.2
1 0 obj
<</FDF
 <</Fields
  [
")
    (loop for (key value) on keys-and-values by #'cddr
       do (format stream "   <</T(~(~a~))/V(~a)>>~%" key
                  (pdf-quote-string (if (stringp value)
                                        value
                                        (format nil "~a" value)))))
    (format stream "  ]
 >>
>>
endobj
trailer
<</Root 1 0 R>>
%%EOF")))
    
