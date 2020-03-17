(defpackage lc.parser.test
  (:use :cl :rove :lc.parser))
(in-package lc.parser.test)

(defmacro with-context-of-string (str &body body)
  `(let ((lc.parser::*context*
           (make-instance 'lc.parser::parsing-context
                          :string ,str)))
     ,@body))

(defun parse-throws (parse-f str error)
  (testing str
    (ok
     (signals
         (with-context-of-string str
           (funcall parse-f))
         error)
     (format nil "throws ~A" error))))

(deftest parse-type-test
  (labels
      ((type-qualifs (parsed-qualifs qualifs)
         (ok (= (length (intersection qualifs parsed-qualifs)) (length qualifs))
             (format nil "type qualifieres: ~A == ~A" parsed-qualifs qualifs)))

       (type-name-and-qualifs (tp name qualifs)
         (with-accessors ((parsed-name :name) (parsed-qualifs :qualifiers)) tp
           (ok (equal parsed-name name) (format nil "type name: ~A == ~A" parsed-name name))
           (type-qualifs parsed-qualifs qualifs)))

       (parsed-type-and-qualifs (str name qualifs)
         (with-context-of-string str
           (type-name-and-qualifs (parse-type) name qualifs)))

       (testing-simple (str name qualifs)
         (testing str
           (parsed-type-and-qualifs str name qualifs)))

       (testing-ptr (str params-for-depth)
         (testing str
           (with-context-of-string str
             (let ((parsed-type (parse-type)))
               (with-accessors ((to-type :to-type)) parsed-type
                 (let ((depth 0)
                       (current-depth-t parsed-type))
                   (tagbody deeper
                      (testing (format nil "pointer depth ~A" depth)
                        (etypecase current-depth-t
                          ;; For pointer types, check their qualifiers and go deeper
                          (ast-pointer-type
                           (destructuring-bind (qualifs) (nth depth params-for-depth)
                             (with-accessors ((parsed-qualifs :qualifiers) (deeper-type :to-type)) current-depth-t
                               (type-qualifs parsed-qualifs qualifs)

                               (incf depth)
                               (setf current-depth-t deeper-type)
                               (go deeper))))

                          ;; For presumably the only normal type, check name and qualifiers and return
                          (ast-simple-type
                           (destructuring-bind (name qualifs) (nth depth params-for-depth)
                             (type-name-and-qualifs current-depth-t name qualifs)))))))))))))

    (testing "simple types are parsed correctly"
      (testing-simple "char" "char" '())
      (testing-simple "const double" "double" '(:const))
      (testing-simple "float const" "float" '(:const))
      (testing-simple "volatile double const" "double" '(:const :volatile))
      (testing-simple "short const" "int" '(:const :short))
      (testing "duplicate qualifiers are removed"
        (testing-simple "long long const const volatile" "int" '(:const :volatile :long-long))
        (testing-simple "const long const long volatile const volatile" "int" '(:const :volatile :long-long)))

      (testing "conflicting modifiers aren't applied"
        (parse-throws 'parse-type "const char long short" 'lc.parser:parsing-error)
        (parse-throws 'parse-type "const char signed unsigned" 'lc.parser:parsing-error)))

    (testing "pointer types are parsed correctly"
      (testing-ptr "char*" '((())
                             ("char" ())))
      (testing-ptr "const char const * volatile const"
                   '(((:const :volatile))
                     ("char" (:const))))
      (testing-ptr "const long * const * const"
                   '(((:const))
                     ((:const))
                     ("int" (:const :long))))

      (testing "pointer markers are not allowed on the left of the main type"
        (parse-throws 'parse-type "* char" 'lc.parser:parsing-error)
        (parse-throws 'parse-type "char * long" 'lc.parser:parsing-error)))))

(deftest parse-function-call-test
  (labels ((parsed-f-check (str name arg-types)
             (testing str
               (with-context-of-string str
                 (let ((parsed-call (parse-function-call)))
                   (with-accessors ((p-name :name) (p-args :arguments)) parsed-call
                     (ok (equal p-name name) (format nil "function name: ~A == ~A" p-name name))
                     (ok (= (length p-args) (length arg-types))
                         (format nil "len(arglist) == len(test param list)"))
                     (loop for p-arg in p-args
                           for arg-type in arg-types
                           for i = 0 then (1+ i)
                           do (testing (format nil "argument ~A" i)
                                (ok (subtypep (type-of p-arg) arg-type)
                                    (format nil "argument type: ~A == ~A" (type-of p-arg) arg-type))))))))))
    (parsed-f-check "func(1, 2.0)" "func" '(ast-integer ast-float))
    (parsed-f-check "func2()" "func2" nil)
    (testing "invalid calls raise errors"
      (parse-throws 'parse-function-call "func(" 'parsing-error)
      (parse-throws 'parse-function-call "func1(1.0," 'parsing-error)
      (parse-throws 'parse-function-call "func2(1.0" 'parsing-error))))

(deftest parse-binary-operator-test
  (labels ((parsed-op-of-string (str test)
             (testing str
               (with-context-of-string str
                 (funcall test (parse-binary-operator)))))
           (is-function-and-named (op name)
             (ok (typep op 'ast-function-call) "parsed as function call")
             (ok (equal (:name op) name) (format nil "operator name is ~A" name)))
           (nth-arg (op n)
             (nth n (:arguments op)))
           (argument-values-are (op expected-args)
             (let ((op-args (:arguments op)))
               (loop for arg-n from 0 to (1- (length op-args))
                     for op-arg = (:value (nth arg-n op-args))
                     for expected-arg = (nth arg-n expected-args)
                     do (ok (equal op-arg expected-arg) (format nil "argument ~A = ~A" (+ arg-n 1) expected-arg))))))
    (parsed-op-of-string "1 + 2" (lambda (op)
                                   (is-function-and-named op "+")
                                   (argument-values-are op '(1 2))))
    (parsed-op-of-string "(1 + 2)" (lambda (op)
                                     (is-function-and-named op "+")
                                     (argument-values-are op '(1 2))))
    (parsed-op-of-string "(1 + (2 + 3))" (lambda (op)
                                           (is-function-and-named op "+")
                                           (ok (= (:value (nth-arg op 0)) 1) "first arg is 1")
                                           (let ((second-arg (nth-arg op 1)))
                                             (testing "second arg"
                                               (is-function-and-named second-arg "+")
                                               (argument-values-are second-arg '(2 3))))))

    (parsed-op-of-string "(1 + (f(21) + 3))" (lambda (op)
                                               (is-function-and-named op "+")
                                               (ok (= (:value (nth-arg op 0)) 1) "first arg is 1")
                                               (let ((second-arg (nth-arg op 1)))
                                                 (testing "second arg"
                                                   (is-function-and-named second-arg "+")
                                                   (testing "first arg"
                                                     (let ((second-arg-arg (nth-arg second-arg 0)))
                                                       (is-function-and-named second-arg-arg "f")
                                                       (argument-values-are second-arg-arg '(21))))
                                                   (ok (= (:value (nth-arg second-arg 1)) 3) "second arg is 3")))))
    (parsed-op-of-string "1 - 2 * 3" (lambda (op)
                                       (is-function-and-named op "-")
                                       (ok (= (:value (nth-arg op 0)) 1) "first arg is 1")
                                       (let ((second-arg (nth-arg op 1)))
                                         (testing "second arg"
                                           (is-function-and-named second-arg "*")
                                           (argument-values-are second-arg '(2 3))))))

    (testing "invalid operators and parens raise errors"
      (parse-throws 'parse-binary-operator ")" 'parsing-error)
      (parse-throws 'parse-binary-operator "(" 'parsing-error)
      (parse-throws 'parse-binary-operator "(1 +" 'parsing-error)
      (parse-throws 'parse-binary-operator "1 + 1)" 'parsing-error)
      (parse-throws 'parse-binary-operator "1 + 1 * 2)" 'parsing-error)
      (parse-throws 'parse-binary-operator "((1 + 1 * 2)" 'parsing-error)
      (parse-throws 'parse-binary-operator "((1 + 1) * 2))" 'parsing-error))))

(deftest parse-if-else-test
  (labels ((parsed-if-else (str cond-val if-val &optional else-val)
             (testing str
               (with-context-of-string str
                 (let ((parsed (parse-if-else)))
                   (ok (= (:value (:condition parsed)) cond-val) (format nil "condition value = ~A" cond-val))

                   (testing "if body"
                     (when (typep if-val 'integer)
                       (setf if-val (list if-val)))
                     (loop for n from 0 below (length if-val)
                           do (ok (= (:value (nth n (:if-body parsed))) (nth n if-val))
                                  (format nil "value #~A is ~A" (1+ n) (nth n if-val)))))
                   (when else-val
                     (testing "else body"
                       (when (typep else-val 'integer)
                         (setf else-val (list else-val)))
                       (loop for n from 0 below (length else-val)
                             do (ok (= (:value (nth n (:else-body parsed))) (nth n else-val))
                                    (format nil "value #~A is ~A" (1+ n) (nth n else-val)))))))))))
    (parsed-if-else "if (1) 2;" 1 2)
    (parsed-if-else "if (1) 2; else 3;" 1 2 3)
    (parsed-if-else "if (1) { 2; 3; }" 1 '(2 3))
    (parsed-if-else "if (1) { 2; 3; } else { 3; 4; }" 1 '(2 3) '(3 4))

    (testing "invalid if statements raise errors"
      (parse-throws 'parse-if-else "if (" 'parsing-error)
      (parse-throws 'parse-if-else "if ()" 'parsing-error)
      (parse-throws 'parse-if-else "if (1) {" 'parsing-error)
      (parse-throws 'parse-if-else "if (1) { else" 'parsing-error)
      (parse-throws 'parse-if-else "if ) { else }" 'parsing-error)
      (parse-throws 'parse-if-else "if (1) { else }" 'parsing-error)
      (parse-throws 'parse-if-else "if (1) { 2 } else { 3 }" 'parsing-error))))
