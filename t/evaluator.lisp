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
       (with-context-of-string "int test1(int x, int y) { return x; }"
         (lc.parser::parse-function-definition)))
      (lc.evaluator::evaluate
       (with-context-of-string "int test2(int x, int y) { return y; }"
         (lc.parser::parse-function-definition)))

      (let ((defined-f1 (gethash "test1" (:functions lc.evaluator:*evaluator-state*)))
            (defined-f2 (gethash "test2" (:functions lc.evaluator:*evaluator-state*))))
        (ng (null defined-f1) "function test1 is defined")
        (ng (null defined-f2) "function test2 is defined")

        (testing "argument order is preserved correctly"

          (run-with-current-state
           (lc.evaluator:translate
            (with-context-of-string "test1(21, 22)"
              (lc.parser::parse-function-call))))

          (ok (= 1 (:stack-pointer lc.evaluator:*evaluator-state*)) "SP = 1 after test1 call")
          (ok (= 21 (:value (aref (:stack lc.evaluator:*evaluator-state*) 0))) "returned value is 21 (first arg)")

          (run-with-current-state
           (lc.evaluator:translate
            (with-context-of-string "test2(21, 22)"
              (lc.parser::parse-function-call))))
          (ok (= 2 (:stack-pointer lc.evaluator:*evaluator-state*)) "SP = 2 after test2 call")
          (ok (= 21 (:value (aref (:stack lc.evaluator:*evaluator-state*) 0))) "returned value is 22 (second arg)"))))))
