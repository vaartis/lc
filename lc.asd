(asdf:defsystem #:lc
  :pathname "src"
  :depends-on (:parse-float :closer-mop)
  :components
  ((:file "memory")
   (:file "parser")))

(asdf:defsystem #:lc/test
  :pathname "t"
  :depends-on (:lc :rove)
  :components
  ((:file "parser")))
