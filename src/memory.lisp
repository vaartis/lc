(defpackage lc.memory
  (:use :cl)
  (:export

   :memory-type
   :memory-value

   :memory-type
   :simple-memory-type
   :pointer-memory-type
   :memory-type-equal

   :put-into-memory
   :remove-from-memory

   :get-standard-simple-type
   :*memory*))
(in-package lc.memory)

(defclass memory-type ()
  ((qualifiers :initarg :qualifiers :accessor :qualifiers))
  (:default-initargs :qualifiers '()))

(defclass simple-memory-type (memory-type)
  ((type-name :initarg :type-name :accessor :type-name)
   (size :initarg :size :accessor :size)))

(defclass pointer-memory-type (memory-type)
  ((pointer-to :initarg :pointer-to :accessor :pointer-to
               :type memory-type)))

(defmethod memory-type-equal (a b)
  nil)
(defmethod memory-type-equal ((a simple-memory-type) (b simple-memory-type))
  (equal (:type-name a) (:type-name b)))
(defmethod memory-type-equal ((a pointer-memory-type) (b pointer-memory-type))
  (memory-type-equal (:pointer-to a) (:pointer-to b)))

(defmethod print-object ((obj simple-memory-type) out)
  (print-unreadable-object (obj out)
    (format out "name: '~A' size: ~A"
            (:type-name obj) (:size obj))))

(defclass memory-value ()
  ((location :initarg :location :accessor :location)
   (value-type :type memory-type :initarg :value-type :accessor :value-type)
   (value :initarg :value :accessor :value))
  (:default-initargs :location nil))

(defmethod print-object ((obj memory-value) out)
  (print-unreadable-object (obj out :type t)
    (format out "~A of type '~A' at address ~A"
            (:value obj) (:value-type obj) (:location obj))))

(defvar +standard-simple-types+
  (make-hash-table :test #'equal))

(defun get-standard-simple-type (name)
  (gethash name +standard-simple-types+))

(defun add-simple-type (name size)
  (setf (gethash name +standard-simple-types+)
        (make-instance 'simple-memory-type
                       :type-name name
                       :size size)))

(eval-when (:load-toplevel :execute)
  (add-simple-type "char" 1)
  (add-simple-type "int" 4)
  (add-simple-type "float" 4))

(defvar *memory* '())

(defun put-into-memory (value)
  (check-type value memory-value)

  (let ((new-memory-location
          (let ((latest-object (loop for val in *memory* maximizing (:location val) return val)))
            (if (not (equal latest-object 0))
                (let* ((latest-object-location (:location latest-object))
                       (latest-object-type (:value-type latest-object))
                       (latest-object-size (:size latest-object-type)))
                  (+ latest-object-location latest-object-size))
                ;; If we couldn't find the object, just return 1
                1))))
    (with-accessors ((loc :location)) value
      (setf loc new-memory-location))
    (push value *memory*)
    ;; Return the same value
    value))

(defun remove-from-memory (value)
  (check-type value memory-value)

  (setf *memory*
        (remove-if (lambda (mem-value)
                     (= (:location value) (:location mem-value)))
                   *memory*)))

(defun put-string-into-memory (str)
  (let ((string-list (coerce str 'list)))
    (dolist (char string-list)
      (put-into-memory (make-instance 'memory-value
                                      :value-type (get-standard-simple-type "char")
                                      :value char)))))
