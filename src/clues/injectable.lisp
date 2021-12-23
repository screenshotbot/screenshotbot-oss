(defpackage :clues/injectable
  (:use #:cl)
  (:import-from #:closer-mop
                #:class-finalized-p
                #:standard-effective-slot-definition
                #:compute-slots
                #:compute-effective-slot-definition
                #:validate-superclass
                #:class-direct-slots
                #:slot-definition-name
                #:slot-value-using-class
                #:class-slots
                #:standard-direct-slot-definition
                #:direct-slot-definition-class)
  (:import-from #:clues/injector
                #:scope-cache
                #:scope
                #:singleton
                #:get-instance
                #:injector)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:injectable
   #:singleton
   #:%injector))
(in-package :clues/injectable)

(defvar *slot* (make-instance
                'standard-effective-slot-definition
                :name '%injector))

(defvar *scopes-slot* (make-instance
                       'standard-effective-slot-definition
                       :name '%scopes))

(defclass injectable (standard-class)
  ((scope :initarg :scope
          :initform nil
          :accessor scope)))

(defmethod compute-slots ((class injectable))
  (let ((default (call-next-method)))
    (append
     (list *slot* *scopes-slot*)
     default)))

(defmethod reinitialize-instance :before ((self injectable) &key scope &allow-other-keys)
  (setf (scope self) scope))

(defmethod validate-superclass ((sub injectable) (superclass standard-class))
  t)

(defclass injectable-object (standard-object)
  ()
  (:metaclass injectable))

(defclass injectable-direct-slot-def (standard-direct-slot-definition)
  ((inject :initarg :inject
           :initform nil
           :accessor dep-inject-class)))

(defmethod compute-effective-slot-definition :around ((class injectable)
                                                      name direct-slots)
  (call-next-method))


(defmethod direct-slot-definition-class ((self injectable) &key
                                         &allow-other-keys)
  'injectable-direct-slot-def)

(defmethod inject ((injector injector)
                   inst
                   slot))

(defmethod inject ((injector injector)
                   inst
                   (slot injectable-direct-slot-def))
  (when (dep-inject-class slot)
   (setf (slot-value inst
                     (slot-definition-name slot))
         (get-instance injector (dep-inject-class slot)))))

(defmethod get-instance ((injector injector)
                         (injectable injectable))
  (get-instance-with-scopes
   injector
   injectable
   (scope injector)))

(defmethod get-instance-with-scopes ((injector injector)
                                     (injectable injectable)
                                     scopes)
  (unless (class-finalized-p injectable)
    (closer-mop:finalize-inheritance injectable))
  (flet ((build ()
           (let ((inst (make-instance injectable)))
             (let ((slots (class-direct-slots injectable)))
               (dolist (slot slots)
                 (inject injector inst slot))
               (setf (slot-value inst '%injector) injector)
               (setf (slot-value inst '%scopes) scopes))
             inst)))
    (let* ((scope-name (car (scope injectable)))
           (scope (loop for x in (reverse scopes)
                        if (typep x scope-name)
                          return x)))
      (cond
        ((and scope-name
              (not scope))
         (error "Could not find scope ~s in scopes ~s" scope-name scopes))
        (scope
         (let ((cache (scope-cache scope)))
           (or
            (gethash injectable cache)
            (setf (Gethash injectable cache)
                  (build)))))
        ((eql nil (scope injectable))
         (build))
        (t (error "invalid scope: ~s" (scope injectable)))))))
