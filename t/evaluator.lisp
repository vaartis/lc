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
      (let ((f1-string "int test1(int x, int y) { return x; }")
            (f2-string "int test2(int x, int y) { return y; }"))
        ;; Evaluate the function definition
        (lc.evaluator::evaluate
         (with-context-of-string f1-string
           (lc.parser::parse-function-definition)))
        (lc.evaluator::evaluate
         (with-context-of-string f2-string
           (lc.parser::parse-function-definition)))

        (let ((defined-f1 (gethash "test1" (:functions lc.evaluator:*evaluator-state*)))
              (defined-f2 (gethash "test2" (:functions lc.evaluator:*evaluator-state*))))
          (ng (null defined-f1) (format nil "~A is defined" f1-string))
          (ng (null defined-f2) (format nil "~A is defined" f2-string))

          (testing "argument order is preserved correctly"
            (let ((f1-call-string "test1(21, 22)")
                  (f2-call-string "test2(21, 22)"))
              (testing f1-call-string
                (run-with-current-state
                 (lc.evaluator:translate
                  (with-context-of-string f1-call-string
                    (lc.parser::parse-function-call))))

                (ok (= 1 (:stack-pointer lc.evaluator:*evaluator-state*)) "SP = 1 after test1 call")
                (ok (= 21 (:value (aref (:stack lc.evaluator:*evaluator-state*) 0))) "returned value is 21 (first arg)"))

              (testing f2-call-string
                (run-with-current-state
                 (lc.evaluator:translate
                  (with-context-of-string f2-call-string
                    (lc.parser::parse-function-call))))
                (ok (= 2 (:stack-pointer lc.evaluator:*evaluator-state*)) "SP = 2 after test2 call")
                (ok (= 21 (:value (aref (:stack lc.evaluator:*evaluator-state*) 0))) "returned value is 22 (second arg)"))))))

      (testing "calling a function as an expression"
        (let ((f3-string "int test3(int x, int y) { return test1(y, x); }"))
          (lc.evaluator::evaluate
           (with-context-of-string f3-string
             (lc.parser::parse-function-definition)))

          (let ((defined-f3 (gethash "test3" (:functions lc.evaluator:*evaluator-state*))))
            (ng (null defined-f3) (format nil "~A is defined" f3-string))

            (let ((f3-call-string "test3(23, 24)"))
              (testing f3-call-string
                (run-with-current-state
                 (lc.evaluator:translate
                  (with-context-of-string f3-call-string
                    (lc.parser::parse-function-call))))

                (ok (= 3 (:stack-pointer lc.evaluator:*evaluator-state*)) "SP = 3 after test3 call")
                (ok (= 24 (:value (aref (:stack lc.evaluator:*evaluator-state*) 3)))
                    "returned value is 24 (second arg, passed to inner function as first arg, which then returns it)")))))))))
