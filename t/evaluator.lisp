(defpackage lc.evaluator.test
  (:use :cl :rove :lc.evaluator)
  (:shadow :run))
(in-package lc.evaluator.test)

(defmacro with-context-of-string (str &body body)
  `(let ((lc.parser::*context*
           (make-instance 'lc.parser::parsing-context
                          :string ,str)))
     ,@body))

(deftest run-test
  (testing "function definition and callling"
    (with-state (make-instance 'lc.evaluator:evaluator-state)
      ;; Evaluate the function definition
      (lc.evaluator::evaluate
       (with-context-of-string "int test(int x) { return x; }"
         (lc.parser::parse-function-definition)))

      (let ((defined-f (gethash "test" (:functions lc.evaluator:*evaluator-state*))))
        (ng (null defined-f) "function test is defined")

        (run-with-current-state
         (lc.evaluator:translate
          (with-context-of-string "test(21)"
            (lc.parser::parse-function-call))))

        (ok (= 1 (:stack-pointer lc.evaluator:*evaluator-state*)) "SP = 1 after call")
        (ok (= 21 (:value (aref (:stack lc.evaluator:*evaluator-state*) 0))) "returned value is 21")))))
