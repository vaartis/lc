(defpackage lc.parser.test
  (:use :cl :rove :lc.parser))
(in-package lc.parser.test)

(defmacro with-context-of-string (str &body body)
  `(let ((lc.parser::*context*
           (make-instance 'lc.parser::parsing-context
                          :string ,str)))
     ,@body))

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
                             (type-name-and-qualifs current-depth-t name qualifs))))))))))))

       (parse-throws (str error)
         (testing str
           (ok
            (signals
                (with-context-of-string str
                  (parse-type))
                error)
            (format nil "parsing ~A throws ~A" str error)))))

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
        (parse-throws "const char long short" 'lc.parser:parsing-error)
        (parse-throws "const char signed unsigned" 'lc.parser:parsing-error)))

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
        (parse-throws "* char" 'lc.parser:parsing-error)
        (parse-throws "char * long" 'lc.parser:parsing-error)))))