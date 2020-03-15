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

    (labels
        ((test-call-result (name def-str call-str expected-sp expected-result-val)
           (lc.evaluator::evaluate
            (with-context-of-string def-str
              (lc.parser::parse-function-definition)))
           (let ((defined-f (gethash name (:functions lc.evaluator:*evaluator-state*))))
             (ng (null defined-f) (format nil "~A is defined" def-str))

             (testing call-str
               (run-with-current-state
                (lc.evaluator:translate
                 (with-context-of-string call-str
                   (lc.parser::parse-function-call))))

               (ok (= expected-sp
                      (:stack-pointer lc.evaluator:*evaluator-state*)) (format nil "SP = ~A after test1 call" expected-sp))
               (ok (= expected-result-val
                      (:value
                       (aref (:stack lc.evaluator:*evaluator-state*) (- expected-sp 1))))
                   (format nil "returned value is ~A" expected-result-val))))))

      (with-state (make-instance 'lc.evaluator:evaluator-state)
        (test-call-result "test1" "int test1(int x, int y) { return x; }" "test1(21, 22)" 1 21)
        (test-call-result "test2" "int test2(int x, int y) { return y; }" "test2(21, 22)" 2 22)

        (testing "calling a function as an expression"
          (test-call-result "test3" "int test3(int x, int y) { return test1(y, x); }" "test3(23, 24)" 3 24))
        (testing "calling an intrinsic int+int"
          (test-call-result "test4" "int test4(int x, int y) { return x + y; }" "test4(21, 9)" 4 30))))))
