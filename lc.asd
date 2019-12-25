(asdf:defsystem #:lc
  :pathname "src"
  :depends-on ("parse-float")
  :components
  ((:file "memory")
   (:file "parser")))
