(asdf:defsystem #:lc
  :pathname "src"
  :depends-on (:parse-float :closer-mop :trivia :lazy)
  :components
  ((:file "parser")
   (:file "memory")
   (:file "runtime-function")
   (:file "intrinsics")
   (:file "evaluator")))

(asdf:defsystem #:lc/test
  :pathname "t"
  :depends-on (:lc :rove)
  :components
  ((:file "parser")
   (:file "evaluator")))
