;;;; Konvertierung vom Sexpr-Snapshot-Format in das Binaerformat.
;;;; Anleitung:
;;;;
;;;; Im laufenden Server mit altem Code:
;;;;   - Einen Snapshot erstellen.  Transaktionsfiles koennen wir nicht laden.
;;;;
;;;; Die Konvertierung findet dann offline statt:
;;;;   - BOS-Branch kompilieren, dieses File laden.
;;;;   - Der Anwendungscode kann geladen sein, muss aber nicht.  Es
;;;;     werden aber die im Snapshot referenzierten Pakete benoetigt.
;;;;     Falls der eigentliche Code nicht geladen ist, einfach ein
;;;;     MAKE-PACKAGE auf alle fehlenden Pakete machen.
;;;;   - (CONVERT-SNAPSHOT "/path/to/datastore/snapshot") aufrufen
;;;;   - Heraus kommt "snapshot.new", das kopiere man nach erfolgreicher
;;;;     Konvertierung in "snapshot" um.
;;;;
;;;; Das hier erstellte Snapshotfile ist noch "unkomprimert".  Nach
;;;; erfolglichem Start der Anwendung teste man den normalen Snapshotvorgang,
;;;; das dann erstelle File muesste etwas kleiner sein.

(in-package :bknr.datastore)

(defvar *layout-counter* 0)

(defun convert-snapshot/encode-layout (class-name slots stream)
  (let ((id (incf *layout-counter*)))
    (%write-tag #\L stream)
    (%encode-integer id stream)
    (%encode-symbol class-name stream)
    (%encode-integer (length slots) stream)
    (dolist (slot slots)
      (%encode-symbol slot stream))
    id))

(defun convert-snapshot/create-object (class objid slots values stream)
  (let ((layout-id (convert-snapshot/encode-layout class nil stream)))
    (%write-tag #\O stream)
    (%encode-integer layout-id stream)
    (%encode-integer objid stream)
    (convert-snapshot/set-slots objid slots values stream)))

(defun convert-snapshot/set-slots (objid slots values stream)
  (let ((layout-id (convert-snapshot/encode-layout 'dummy slots stream)))
    (%write-tag #\S stream)
    (%encode-integer layout-id stream)
    (%encode-integer objid stream)
    (dolist (value values)
      (encode value stream))))

(defun convert-snapshot/exp (exp out)
  (declare (optimize (speed 3)))
  (if (consp exp)
      (case (car exp)
        (create-object
         (destructuring-bind (class &rest initargs) (cdr exp)
           (loop with id = nil
              for (slot value) on initargs by #'cddr
              collect slot into slots
              collect (convert-snapshot/exp value out) into values
              do
              (when (eq slot :id)
                (setf id value))
              finally (convert-snapshot/create-object class id slots values out))))
        (set-slots
         (destructuring-bind (obj &rest initargs) (cdr exp)
           (loop for (slot value) on initargs by #'cddr
              collect slot into slots
              collect (convert-snapshot/exp value out) into values
              finally (convert-snapshot/set-slots obj slots values out))))
        (store-object-with-id
         (let ((o (allocate-instance (find-class 'store-object))))
           (setf (slot-value o 'id) (second exp))
           o))
        (t
         (eval exp)))
      exp))

(defun convert-snapshot (file)
  (with-store-state (:restore)
    (with-open-file (in file)
      (with-open-file (out (make-pathname :type "new" :defaults file)
                           :direction :output
                           :element-type '(unsigned-byte 8))
        (let ((*package* #.*package*))
          (loop for exp = (read in nil 'eof nil)
             until (eql exp 'eof)
             do (convert-snapshot/exp exp out)))))))
