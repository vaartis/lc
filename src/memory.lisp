(defpackage lc.memory
  (:use :cl))
(in-package lc.memory)

(defclass memory-type ()
  ((type-name :accessor type-name :initarg :type-name)
   (size :accessor size :initarg :size)))

(defmethod print-object ((obj memory-type) out)
  (print-unreadable-object (obj out)
    (format out "name: ~A size: ~A"
            (type-name obj) (size obj))))

(defclass memory-value ()
  ((value-type :type memory-type :accessor value-type :initarg :value-type)
   (location :accessor location :initarg :location)
   (value :accessor value :initarg value :initarg :value)))

(defmethod print-object ((obj memory-value) out)
  (print-unreadable-object (obj out :type t)
    (format out "~A of type ~A at address ~A"
            (value obj) (value-type obj) (location obj))))

(defvar +char-type+
  (make-instance 'memory-type
                 :type-name :char
                 :size 1))

(defvar *memory* (make-list 0))

(defun put-string-into-memory (str)
  (let ((string-list (coerce str 'list)))
    (dolist (char string-list)
      (let ((new-memory-location
              (let ((latest-object (loop for val in *memory* maximizing (location val) return val)))
                (if (not (equal latest-object 0))
                    (let* ((latest-object-location (location latest-object))
                           (latest-object-type (value-type latest-object))
                           (latest-object-size (size latest-object-type)))
                      (+ latest-object-location latest-object-size))
                    ;; If we couldn't find the object, just return 1
                    1))))
        (push (make-instance 'memory-value
                             :value-type +char-type+
                             :location new-memory-location
                             :value char)
              *memory*)))))
