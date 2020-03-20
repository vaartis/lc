(defpackage lc.evaluator.runtime-function
  (:use :cl :lc.memory)
  (:export :runtime-function))
(in-package lc.evaluator.runtime-function)

(defclass runtime-function ()
  ((name :initarg :name :accessor :name)
   (arguments :initarg :arguments :accessor :arguments)
   (body :initarg :body :accessor :body
         :type (or vector function))
   (return-type :initarg :return-type :accessor :return-type
                :type memory-type)
   (label-table :initarg :label-table :accessor :label-table
                :type hash-table))
  (:default-initargs :body nil :label-table (make-hash-table :test #'equal)))
