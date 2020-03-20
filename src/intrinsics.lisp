(defpackage lc.evaluator.intrinsics
  (:use :cl :alexandria :trivia
   :lc.memory :lc.parser :lc.evaluator.runtime-function)
  (:export :dispatch-intrinsic :intrinsic-from-actual-name))
(in-package lc.evaluator.intrinsics)

(defun dispatch-intrinsic (dispatched-name args)
  (let ((maybe-intrinsic-dispatcher (assoc-value +intrinsic-function-dispatchers+ dispatched-name :test #'equal)))
    (when maybe-intrinsic-dispatcher
      (funcall maybe-intrinsic-dispatcher args))))

(defun intrinsic-from-actual-name (name)
  (assoc-value (lazy:lazy-value +intrinsic-functions+) name :test #'equal))

(defmacro operator-intrinsic-dispatcher (op &body clauses)
  `(cons ,op
         (lambda (args)
           (when (= (length args) 2)
             (let ((lhs-type (type-of-ast (nth 0 args)))
                   (rhs-type (type-of-ast (nth 1 args))))
               (match (cons (:type-name lhs-type) (:type-name rhs-type))
                 ,@(loop for clause in clauses
                         collect (destructuring-bind (rhs-t lhs-t) clause
                                   `((cons ,rhs-t ,lhs-t)
                                     (intrinsic-from-actual-name ,(format nil "~A~A~A" rhs-t op lhs-t)))))))))))

(defvar +intrinsic-function-dispatchers+
  (list
   (operator-intrinsic-dispatcher "+"
     ("int" "int"))
   (operator-intrinsic-dispatcher "-"
     ("int" "int"))
   (operator-intrinsic-dispatcher "/"
     ("int" "int"))
   (operator-intrinsic-dispatcher "%"
     ("int" "int"))
   (operator-intrinsic-dispatcher "*"
     ("int" "int"))))

(defmacro lambda-op-intrinsic (name (lhs-type rhs-type ret-type) (lhs-name rhs-name) &body body)
  `(progn
     (cons ,name
           (make-instance 'runtime-function
                          :name ,name
                          :return-type ,ret-type
                          :arguments `(("lhs" . ,,lhs-type)
                                       ("rhs" . ,,rhs-type))
                          :body (lambda (,lhs-name ,rhs-name)
                                  ,@body)))))

(defvar +intrinsic-functions+
  ;; Has to be lazy, because if it's evaluated on load, the memory types aren't there yet
  (lazy:lazy
    (list (lambda-op-intrinsic "int+int" (+int-t+ +int-t+ +int-t+) (lhs rhs)
            (make-instance 'ast-integer
                           :value (+ (:value lhs) (:value rhs))))
          (lambda-op-intrinsic "int-int" (+int-t+ +int-t+ +int-t+) (lhs rhs)
            (make-instance 'ast-integer
                           :value (- (:value lhs) (:value rhs))))
          (lambda-op-intrinsic "int/int" (+int-t+ +int-t+ +int-t+) (lhs rhs)
            (make-instance 'ast-integer
                           :value (floor (:value lhs) (:value rhs))))
          (lambda-op-intrinsic "int%int" (+int-t+ +int-t+ +int-t+) (lhs rhs)
            (multiple-value-bind (div remainder) (floor (:value lhs) (:value rhs))
              (declare (ignore div))
              (make-instance 'ast-integer
                             :value remainder)))
          (lambda-op-intrinsic "int*int" (+int-t+ +int-t+ +int-t+) (lhs rhs)
            (make-instance 'ast-integer
                           :value (* (:value lhs) (:value rhs)))))))
