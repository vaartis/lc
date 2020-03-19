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
        ((test-call-result (name def-str call-str expected-result-val)
           (with-state (make-instance 'lc.evaluator:evaluator-state)
             (with-context-of-string def-str
               (mapcar #'lc.evaluator::evaluate (lc.parser::parse-toplevels)))

             (let ((defined-f (gethash name (:functions lc.evaluator:*evaluator-state*))))
               (ng (null defined-f) (format nil "~A is defined" name))

               (testing call-str
                 (run-with-current-state
                  (lc.evaluator:translate
                   (with-context-of-string call-str
                     (lc.parser::parse-function-call))))


                 (ok (= expected-result-val
                        (:value
                         (aref (:stack lc.evaluator:*evaluator-state*) 0)))
                     (format nil "returned value is ~A" expected-result-val))))))
         (test-definition-throws (def-str condition)
           (ok
            (signals
                (lc.evaluator::evaluate
                 (with-context-of-string def-str
                   (lc.parser::parse-function-definition)))
                condition)
            (format nil "defining ~A throws ~A" def-str condition))))

      (with-state (make-instance 'lc.evaluator:evaluator-state)
        (test-call-result "test1" "int test1(int x, int y) { return x; }" "test1(21, 22)" 21)
        (test-call-result "test2" "int test2(int x, int y) { return y; }" "test2(21, 22)" 22)

        (testing "calling a function as an expression"
          (test-call-result "test3"
                            "int test1(int x, int y) { return x; } int test3(int x, int y) { return test1(y, x); }"
                            "test3(23, 24)" 24))
        (testing "calling intrinsics"
          (test-call-result "test_add" "int test_add(int x, int y) { return x + y; }" "test_add(21, 9)" 30)
          (test-call-result "test_sub" "int test_sub(int x, int y) { return x - y; }" "test_sub(20, 10)" 10)
          (test-call-result "test_div" "int test_div(int x, int y) { return x / y; }" "test_div(20, 10)" 2)
          (test-call-result "test_rem" "int test_rem(int x, int y) { return x % y; }" "test_rem(1, 2)" 1)
          (test-call-result "test_mul" "int test_mul(int x, int y) { return x * y; }" "test_mul(2, 10)" 20)
          (test-call-result "test_complex" "int test_complex(int x, int y) { return (x + (y * 2) / 3) - 1; }" "test_complex(2, 10)" 7)
          (test-call-result "test_complex2" "int test_complex2(int x, int y) { return x + y * 2 / 3 - 1; }" "test_complex2(2, 10)" 7))
        (testing "variable definition"
          (test-call-result "test_def" "int test_def(int x) { int y = 2; int z = y + x; return z * 2;}" "test_def(10)" 24))
        (testing "multiple definition causes an error"
          (test-definition-throws "int test_multiple_def(int x) { int x = 1; }" 'lc.evaluator::variable-already-declared))
        (testing "if and else"
          (test-call-result "test_if" "int test_if(int x) { bool y = true; if (y) return x; }" "test_if(84)" 84)
          (test-call-result "test_if_else" "int test_if_else(int x) { bool y = false; if (y) return x; else return 0; }" "test_if_else(84)" 0)
          (test-call-result "test_nested_if_else"
                            "int test_nested_if_else(int x) {
bool y = false;
bool z = true;
int result;
if (y)
  result = 0;
else
  if (z)
    result = 1;
  else
    result = 2;

return result;
}" "test_nested_if_else(84)" 1))))))
