(defpackage lc.parser
  (:use :cl :trivia)
  (:shadow :get :symbol)
  (:export :parse-file :parse-type
           :parse-function-call :parse-binary-operator :parse-if-else :parse-toplevel :parse-toplevels
           :parsing-error))
(in-package lc.parser)

(defclass macro-definition ()
  ((name :initarg :name :accessor :name)
   (args :initarg :arguments :accessor :arguments)
   (value :initarg :value :accessor :value)))

(defclass parsing-context ()
  ((inner-string :initarg :string :accessor :inner-string)
   (position :initarg :position :accessor :position))
  (:default-initargs :position 0))

(defun save-position-data ()
  (:position *context*))

(defun load-position-data (pos)
  (setf (:position *context*) pos))

(defvar *context*)

(defun make-preproc-macro (name value &key args)
  (make-instance 'macro-definition
                 :name name
                 :arguments args
                 :value value))

(defun get (&optional (advance-by 1))
  "Get the next character and advance the position"
  (let ((val (peek)))
    (with-slots (position symbol line) *context*
      (incf position advance-by)

      val)))

(defun back (&optional (n 1))
  ;; Go N characters back
  (decf (:position *context*) n))

(defun peek (&optional (peek-by 0))
  "Get the next character without advancing the position"
  (with-slots (position inner-string) *context*
    (if (< (+ position peek-by) (length inner-string))
        (let ((curr-val (aref inner-string (+ position peek-by))))
          curr-val)
        nil)))

(defun peek-nonempty (&optional (peek-by 0))
  "Get the next non-empty character without advancing the position"
  (loop for skipped-chars = peek-by then (1+ skipped-chars)
        for current-char = (peek skipped-chars)
        when (or (null current-char) ; If we're at the end just return nil
                 (not (find current-char *skip-chars*)))
          return current-char))

(defun peek-nonempty-string (&key (peek-by 0) (only-chars nil only-chars-provided) (max-length nil max-length-provided))
  "Peek the next non-empty string"
  (let (found-nonempty result)
    (loop for skipped-chars = peek-by then (1+ skipped-chars)
          for current-char = (peek skipped-chars)
          do (progn
               (unless (find current-char *skip-chars*)
                 (unless found-nonempty
                   (setf found-nonempty t)))

               ;; Exit if the file ended or there's a space after the string has already been found
               (when (or (null current-char)
                         (and found-nonempty
                              (find current-char *skip-chars*))
                         (and only-chars-provided
                              found-nonempty
                              (not (find current-char only-chars)))
                         (and max-length-provided
                              (>= (length result) max-length)))
                 (loop-finish))
               (unless (find current-char *skip-chars*)
                 (push current-char result))))
    (unless (null result)
      (concatenate 'string (reverse result)))))

(defun peek-ident ()
  "Peek the next identifier or anything that can be considered one (such as keywords)"
  (let ((collected-word
          (loop for skipped-chars = 0 then (1+ skipped-chars)
                for current-char = (peek skipped-chars)
                while (and (not (null current-char))
                           (if (null collected-word)
                               (first-character-p current-char)
                               (ident-character-p current-char)))
                collect current-char into collected-word
                finally (return collected-word))))
    (if (null collected-word)
        collected-word
        (concatenate 'string collected-word))))

(defun parse-number ()
  "Peek next to see if something is a number"
  (let ((first-char (peek)))
    (unless (numeric-char-p first-char)
      (return-from parse-number))

    ;; Skip the first character now
    (get)

    (let*
        ((rest-list (loop for ch = (peek)
                          while (or (numeric-char-p ch)
                                    (equal ch #\.))
                          collect (progn
                                    (get)
                                    ch)))
         (rest (coerce rest-list 'string)))
      (let ((full-number (format nil "~A~A" first-char rest)))
        (if (find #\. full-number)
            (make-instance 'ast-float
                           :value (parse-float:parse-float full-number))
            (make-instance 'ast-integer
                           :value (parse-integer full-number)))))))

(defun expect (expected)
  "Get the next character and raise an error if it's not the expected character"
  (let* ((char (get))
         (expect-result
           (etypecase expected
             (standard-char (equal char expected))
             (cons (some (lambda (ch) (equal char ch)) expected)))))
    (unless expect-result
        (parsing-error "Expected ~A, but found '~A'"
                       (typecase expected
                         (standard-char (format nil "'~A'" expected))
                         (cons (format nil "~{'~A'~^ or ~}" expected)))
                       char))))

(defun expect-ident (expected)
  (let ((parsed (parse-ident)))
    (unless (equal parsed expected)
      (parsing-error "Expected ~A, but found '~A'" expected parsed))))

(define-condition parsing-error (error)
  ((message :initarg :message)
   (context :initarg :context))
  (:report (lambda (condition stream)
             (with-slots (message context) condition
               ;; Count the symbol and line at position
               (let ((symbol 0) (line 1))
                 (with-accessors ((string :inner-string) (position :position)) context
                   (loop for n from 0 below position
                         until (>= n (length string))
                         for curr-char = (aref string n)
                         do (if (equal curr-char #\Newline)
                                (progn
                                  (setf symbol 1)
                                  (incf line))
                                (incf symbol))))
                 (if (>= position (length string))
                     ;; Put the symbol onto the end
                     (incf symbol))

                 (format stream "~A at line ~A, symbol ~A~%" message line symbol)
                 (let* ((line-specifier (format nil "~A| " line))
                        (line-specifier-length (length line-specifier)))
                   (format stream "~A~A~%" line-specifier (get-line-at line context))
                   ;; Write a ^ showing the position, skipping the first
                   (loop repeat (1- (+ line-specifier-length symbol)) do (write-char #\Space stream))
                   (write-char #\^ stream)))))))

(defun get-line-at (line-num context)
  (with-slots (inner-string) context
    (loop for position = 0 then (1+ position)
          while (< position (length inner-string))
          for current-char = (aref inner-string position)
          for found-lines = 1 then (if (equal current-char #\Newline)
                                       (1+ found-lines)
                                       found-lines)
          when (= found-lines line-num)
            return (let ((substr-end (position #\Newline inner-string :start (1+ position))))
                     (subseq inner-string (if (> found-lines 1) (1+ position) position) substr-end)))))

(defun parsing-error (&rest format-args)
  (with-slots (symbol line) *context*
    (unless (null format-args)
      (push nil format-args))
    (error 'parsing-error
           :message (apply #'format format-args)
           :context *context*)))

(defvar *skip-chars* '(#\Newline #\Space))
(defun skip-empty ()
  "Skips empty tokens like spaces and newlines"
  (loop for ch = (peek)
        while (find ch *skip-chars*)
        do (get)))

(defun first-character-p (char)
  (or (alpha-char-p char) (equal char #\_)))

(defun ident-character-p (char)
  (when (null char)
    (return-from ident-character-p nil))

  (or (alphanumericp char) (equal char #\_)))

(defvar +numeric-chars+ (coerce "1234567890" 'list))
(defun numeric-char-p (char)
  (find char +numeric-chars+ :test #'equal))

(defun parse-ident ()
  (let ((first-char (get)))
    (when (null first-char)
      (parsing-error "Expected a character to begin an identifier, but reached the end of file"))
    (unless (or (alpha-char-p first-char) (equal first-char #\_))
      (parsing-error "Expected the first identifier character to be an alpha character or an underscore, but got '~A'" first-char))

    (let* ((rest-list (loop for ch = (peek)
                            while (ident-character-p ch)
                            collect (progn
                                      (get)
                                      ch)))
           (rest (coerce rest-list 'string)))
      (format nil "~A~A" first-char rest))))

(defclass ast () ())

(defclass ast-type (ast)
  ((qualifiers :initarg :qualifiers :accessor :qualifiers))
  (:default-initargs :qualifiers nil))

(defclass ast-simple-type (ast-type)
  ((name :initarg :name :accessor :name
         :documentation "The name of the type")))

(defclass ast-pointer-type (ast-type)
  ((to-type :initarg :to-type :accessor :to-type
            :type ast-type))
  (:default-initargs :to-type nil))

(defclass ast-function-declaration (ast)
  ((return-type :initarg :return-type :accessor :return-type)
   (name :initarg :name :accessor :name)
   (arguments :initarg :arguments :accessor :arguments)))

(defclass ast-function-definition (ast)
  ((function-declaration :initarg :function-declaration :accessor :function-declaration
                         :type ast-function-declaration)
   (body :initarg :body :accessor :body)))


(defclass ast-statement (ast) ())

(defclass ast-return (ast-statement)
  ((value :initarg :value :accessor :value
          :type ast-value)))

(defclass ast-variable-declaration (ast-statement)
  ((decl-type :initarg :decl-type :accessor :decl-type
              :type ast-type)
   (name :initarg :name :accessor :name)))

(defclass ast-variable-definition (ast-statement)
  ((var-decl :initarg :var-decl :accessor :var-decl
             :type ast-variable-declaration)
   (value :initarg :value :accessor :value
          :type ast-value)))

(defclass ast-variable-assignment (ast-statement)
  ((name :initarg :name :accessor :name)
   (value :initarg :value :accessor :value)))

(defmethod print-object ((obj ast-variable-assignment) out)
  (print-unreadable-object (obj out)
    (format out "variable assignment ~A = ~A"
            (:name obj) (:value obj))))

(defclass ast-pointer-assignment (ast-variable-assignment)
  ;; TODO: figure something better out if this doesn't work well
  ((depth :initarg :depth :accessor :depth)))

(defclass ast-if-else (ast-statement)
  ((condition :initarg :condition :accessor :condition)
   (if-body :initarg :if-body :accessor :if-body)
   (else-body :initarg :else-body :accessor :else-body))
  (:default-initargs :else-body nil))

(defclass ast-value (ast) ())

(defclass ast-integer (ast-value)
  ((value :initarg :value :accessor :value)))

(defmethod print-object ((obj ast-integer) out)
  (print-unreadable-object (obj out)
    (format out "integer ~A"
            (:value obj))))

(defclass ast-float (ast-value)
  ((value :initarg :value :accessor :value)))

(defclass ast-bool (ast-value)
  ((value :initarg :value :accessor :value)))

(defclass ast-variable-value (ast-value)
  ((name :initarg :name :accessor :name)))

(defmethod print-object ((obj ast-variable-value) out)
  (print-unreadable-object (obj out)
    (format out "variable-value ~A"
            (:name obj))))

(defclass ast-variable-reference-value (ast-variable-value) ())

(defclass ast-variable-dereference-value (ast-variable-value)
  ((depth :initarg :depth :accessor :depth)))

(defclass ast-function-call (ast-value)
  ((name :initarg :name :accessor :name)
   (arguments :initarg :arguments :accessor :arguments)))

(defmethod print-object ((obj ast-function-call) out)
  (print-unreadable-object (obj out)
    (format out "call ~A~A"
            (:name obj) (:arguments obj))))

(eval-when (:load-toplevel :execute)
  (labels ((all-subclasses (class-name)
             (let* ((class (find-class class-name))
                    (subclasses (closer-mop:class-direct-subclasses class)))
               (alexandria:flatten
                (concatenate
                 'list
                 (list class)
                 (map 'list (lambda (class)
                              (let ((subclasses
                                      (all-subclasses (class-name class))))
                                (if (null subclasses) class subclasses)))
                      subclasses))))))
    (export (map 'list #'class-name (all-subclasses 'ast)))))

(defun parse-pointer-type (type)
  "Parses a pointer type, assuming that the it has already been determined
   that the type is a pointer and the current character is placed after the poiner marker"
  (let ((pointer-type
          (make-instance 'ast-pointer-type
                         :to-type type)))
    ;; If there are more pointer markers, make a nested pointer type
    (if (equal (peek-nonempty) #\*)
        (progn
          ;; Skip the empty tokens and the current pointer marker
          (skip-empty)
          (get)

          ;; Recursively make a pointer to a pointer
          (parse-pointer-type pointer-type))
        ;; Otherwise just return the created type
        pointer-type)))

(defun str-to-keyword (str)
  (intern (string-upcase str) 'keyword))

(defvar +simple-type-main-words+ '("void" "char" "int" "float" "double" "bool"))
(defvar +type-modifier-words+ '("volatile" "const" "restrict"))
(defvar +number-type-length-words+ '("short" "long"))
(defvar +number-type-sign-words+ '("signed" "unsigned"))
(defun number-type-modifier-words () (concatenate 'list
                                                  +number-type-length-words+
                                                  +number-type-sign-words+))

(defvar *should-unwind* nil)
(defmacro def-unwindable-parser (name (&rest params) &body body)
  "Defines a parser function that, if a parsing-error is signaled, will unwind the parser
   and return nil. Otherwise works like a normal defun."
  (let ((unwind-position-name (gensym "UNWIND-POSITION"))
        (body-name (gensym "BODY")))
    `(defun ,name (,@params)
       (flet ((,body-name ()
                ,@body))
         (if *should-unwind*
             (let ((,unwind-position-name (slot-value *context* 'position)))
               (handler-case
                   (,body-name)
                 (parsing-error ()
                   (setf (slot-value *context* 'position) ,unwind-position-name)
                   nil)))
             (,body-name))))))

(defun parse-type ()
  (let ((type-parts ())
        (found-main-word nil))

    (tagbody parse-next
       (let ((next-char (peek)))
         (cond
           ((equal next-char #\*)

            (get)
            (skip-empty)

            (push "*" type-parts)

            (go parse-next))
           ((find (peek-ident) +simple-type-main-words+ :test #'equal)
            (let ((main-word (parse-ident)))
              (when found-main-word
                (parsing-error "Main type was already found, but another one was encountered: ~A" main-word))

              (setf found-main-word t)

              (push main-word type-parts)
              (skip-empty)

              (go parse-next)))
           ((find (peek-ident) (concatenate 'list +type-modifier-words+ (number-type-modifier-words)) :test #'equal)

            (push (parse-ident) type-parts)
            (skip-empty)

            (go parse-next)))))

    (let ((current-modifiers '())

          number-length-modifier
          number-sign-modifier

          main-type
          result-type)
      (loop for word in type-parts
            do (cond
                 ((find word +simple-type-main-words+ :test #'string=)
                  (setf main-type word))


                 ((find word +type-modifier-words+ :test #'string=)
                  (let ((kw (str-to-keyword word)))
                    (unless (find kw current-modifiers)
                      (push kw current-modifiers))))

                 ((find word +number-type-length-words+ :test #'string=)
                  (unless (or (null number-length-modifier)
                              (equal number-length-modifier :long))
                    (parsing-error "Length specifiers '~A' and '~A' don't make sense together"
                                   number-length-modifier (str-to-keyword word)))

                  (setf number-length-modifier
                        (if (equal number-length-modifier :long)
                            ;; If the previous modifer was long, turn the effective modifier into long long
                            :long-long
                            (str-to-keyword word))))


                 ((find word +number-type-sign-words+ :test #'string=)
                  (unless (null number-sign-modifier)
                    (parsing-error "Two number sign specifiers don't make sense"))
                  (setf number-sign-modifier (str-to-keyword word)))


                 ((equal word "*")
                  (unless (and (null number-sign-modifier) (null number-length-modifier))
                    (parsing-error "Number modifiers do not make sense on a poiter type"))

                  (when main-type
                    (parsing-error "Cannot specify a pointer marker on left of the main type"))

                  (etypecase result-type
                    (null
                     (setf result-type
                           (make-instance 'ast-pointer-type
                                          :qualifiers current-modifiers))
                     (setf current-modifiers nil))
                    (ast-pointer-type
                     (with-accessors ((to-type :to-type)) result-type
                       (setf to-type (make-instance 'ast-pointer-type
                                                    :qualifiers current-modifiers))
                       (setf current-modifiers nil)))))))

      (when (and (null main-type) (not (null number-length-modifier)))
        (setf main-type "int"))
      (when (null main-type)
        (parsing-error "There was no main type in the type declaration"))

      (let ((main-type-qualifiers current-modifiers))
        (when number-sign-modifier (push number-sign-modifier main-type-qualifiers))
        (when number-length-modifier (push number-length-modifier main-type-qualifiers))

        (let ((main-type-constructed
                (make-instance 'ast-simple-type
                               :name main-type
                               :qualifiers main-type-qualifiers)))
          (etypecase result-type
            (null
             (setf result-type main-type-constructed))

            (ast-pointer-type
             (labels ((assign-main-to-pointer (pointer main)
                        (with-accessors ((to-type :to-type)) pointer
                          (if (null to-type)
                              (setf to-type main)
                              (assign-main-to-pointer to-type main)))))
               (assign-main-to-pointer result-type main-type-constructed))))

          result-type)))))

(defun parse-function-header-args ()
  "Parses the function header, including the parens. Assumes that the current point is at the opening paren"
  (expect #\()
  (skip-empty)

  (let ((return-args
          (if (not (equal (peek) #\)))
              (let ((arguments '()))
                (tagbody parse-arg
                   (let ((type (parse-type)))
                     (skip-empty)
                     (let ((name (parse-ident)))
                       (push (cons name type) arguments)

                       (skip-empty)
                       (when (equal (peek) #\Comma)
                         ;; If there is a comma afterwards, skip it, skip empty space and parse the next argument too
                         (get)
                         (skip-empty)

                         (go parse-arg)))))
                ;; Reverse the argument list because push works backwards
                (reverse arguments))
              nil)))
    (expect #\))
    return-args))

(defun parse-function-definition ()
  (let ((ret-type (parse-type)))
    (skip-empty)
    (let ((f-name (parse-ident)))
      (skip-empty)
      (let* ((args (parse-function-header-args))
             (f-decl (make-instance 'ast-function-declaration
                                   :return-type ret-type
                                   :name f-name
                                   :arguments args)))
        (skip-empty)
        (let ((current-char (peek)))
          (case current-char
              ;; If the declaration ends with a semicolumn, it's just a declaration
              (#\; f-decl)
            (#\{ (let ((f-body (parse-block-body)))
                   (make-instance 'ast-function-definition
                                  :function-declaration f-decl
                                  :body f-body)))
            (otherwise (parsing-error "Expected '{' or ';' after the '~A' function declaration, but found ~A" f-name current-char))))))))

(defun parse-block-body ()
  (expect #\{)

  (let ((statements '()))
    ;; Loop while next char is not a closing bracket,
    ;; pushing statements into the statements list
    (loop while (progn
                  (skip-empty)
                  (not (equal (peek) #\})))
          do (push (parse-statement) statements))
    ;; Skip the } at the end
    (expect #\})

    ;; Revers the statement list since push works backwards
    (reverse statements)))

(defun parse-if-else ()
  (expect-ident "if")
  (skip-empty)
  (expect #\()
  (let ((cond-expr (parse-expression)))
    (expect #\))
    (skip-empty)

    (let ((if-body
            ;; If there's a {, parse as a block. If not, then parse only one statement
            (if (equal (peek) #\{)
                (parse-block-body)
                (list (parse-statement)))))
      (skip-empty)
      ;; If there's an else block, also parse it. It will be nil if there isn't one
      (let ((else-body
              (when (equal (peek-ident) "else")
                (expect-ident "else")
                (skip-empty)

                (if (equal (peek) #\{)
                    (parse-block-body)
                    (list (parse-statement))))))
        (make-instance 'ast-if-else
                       :condition cond-expr
                       :if-body if-body
                       :else-body else-body)))))

(defun parse-statement ()
  (let* ((next-ident (peek-ident))
         (needs-semicolumn t)
         (statement
           (cond
             ((equal next-ident "if")
              (setf needs-semicolumn nil)
              (parse-if-else))

             ((string= next-ident "return")
              (parse-ident)
              (skip-empty)
              (make-instance 'ast-return
                             :value (parse-expression)))
             ((or
               (equal (peek) #\*)
               (let ((ident-length (length next-ident)))
                 (let ((char-after-ident (peek-nonempty ident-length)))
                   ;; TODO: handle *= /= etc.
                   (equal char-after-ident #\=))))
              (parse-variable-assignment))
             ;; As a last case, try parsing as a variable definition
             (t
              (let* ((*should-unwind* t)
                     (maybe-parsed-variable (parse-variable-definition)))
                (if maybe-parsed-variable
                    maybe-parsed-variable
                    ;; Otherwise, parse as an expression
                    (parse-expression))))
             ;; (t (parsing-error "Expected an identifier at the beinning of the statement, but found ~A" next-ident))
             )))
    (skip-empty)
    (when needs-semicolumn
      (expect #\;))

    statement))

(defun parse-variable-assignment ()
  (let ((depth 0))
    (when (equal (peek) #\*)
      (tagbody more-depth
         (when (equal (peek) #\*)
           (get)
           (skip-empty)
           (incf depth)
           (go more-depth))))

    (let ((var-name (parse-ident)))
      (skip-empty)
      (expect #\=)
      (skip-empty)
      (let ((var-value (parse-expression)))
        (if (zerop depth)
            (make-instance 'ast-variable-assignment
                           :name var-name
                           :value var-value)
            (make-instance 'ast-pointer-assignment
                           :name var-name
                           :value var-value
                           :depth depth))))))

(def-unwindable-parser parse-variable-definition ()
  (let ((var-type (parse-type)))
    (skip-empty)
    (let ((var-name (parse-ident)))
      (let ((var-decl (make-instance 'ast-variable-declaration
                                 :name var-name
                                 :decl-type var-type)))

        (skip-empty)
        (let ((next-char (peek)))
          (case next-char
            ;; Return the declaration if there is no assignment afterwards
            ;; Don't touch the ";", it will be checked by parse-statement
            (#\; var-decl)
            (#\=
             ;; Skip the = and everything afterwards
             (get)
             (skip-empty)
             (make-instance 'ast-variable-definition
                            :var-decl var-decl
                            :value (parse-expression)))
            (t
             (parsing-error "Expected ; or = for variable declaration, but got ~A" next-char))))))))

(defun parse-function-call ()
  (let ((f-name (parse-ident)))
    (skip-empty)
    (expect #\()
    (let ((f-args '()))
      (tagbody next-arg
         (skip-empty)
         (unless (equal (peek) #\))
           (push (parse-expression) f-args)
           (skip-empty)
           (let ((next-char (peek)))
             (expect '(#\, #\)))
             (when (equal next-char #\,)
               (go next-arg)))))
      (make-instance 'ast-function-call
                     :name f-name
                     :arguments (reverse f-args)))))

(defclass operator ()
  ((name :initarg :name :accessor :name)
   (precedence :initarg :precedence :accessor :precedence)
   (is-r :initarg :is-r :accessor :is-r)))

(defmethod print-object ((obj operator) out)
  (print-unreadable-object (obj out)
    (format out "operator ~A"
            (:name obj))))

(defun make-operator (name precedence &optional is-r)
  ;; 15 is the total amount of different precedences in C, +1 to make just using minus here work
  ;; Reference: https://en.cppreference.com/w/c/language/operator_precedence
  (make-instance 'operator :name name :precedence (- 16 precedence) :is-r is-r))

(defvar +binary-operators+
  (list
   (make-operator "*" 3)
   (make-operator "/" 3)
   (make-operator "%" 3)

   (make-operator "+" 4)
   (make-operator "-" 4)

   (make-operator "<" 6)
   (make-operator "<=" 6)
   (make-operator ">" 6)
   (make-operator ">=" 6)

   (make-operator "&&" 11)
   (make-operator "||" 12)

   ;; This isn't really right, but it makes things work
   (make-operator "(" 15)
   (make-operator ")" 15)))

(def-unwindable-parser parse-binary-operator ()
  (let ((operator-chars (delete-duplicates
                         (apply #'concatenate
                                (append '(vector)
                                        (mapcar #':name  +binary-operators+)))))
        result-queue operator-stack)
    (tagbody repeat
       (let ((next-nonempt (peek-nonempty-string :only-chars operator-chars)))
         (cond
           ((equal (peek-nonempty-string :only-chars "(" :max-length 1) "(")

            (push (find "(" +binary-operators+ :key #':name :test #'equal) operator-stack)

            (skip-empty)
            (get)

            (go repeat))
           ((equal (peek-nonempty-string :only-chars ")" :max-length 1) ")")

            (let (matched)
              (loop while (and (not (null operator-stack))
                               (string-not-equal (:name (car operator-stack)) "("))
                    do (progn
                         (push (pop operator-stack) result-queue)
                         (setf matched t)))
              (unless (and matched (car operator-stack) (equal (:name (car operator-stack)) "("))
                (parsing-error "Mispatched parens"))
              (pop operator-stack)

              (when (and (not matched) (null operator-stack))
                (parsing-error "Mismatching parens"))

              (skip-empty)
              (get)

              (go repeat)))
           ((find next-nonempt +binary-operators+ :key #':name :test #'equal)
            (let ((o1 (find next-nonempt +binary-operators+ :key #':name :test #'equal)))
              (loop until (null operator-stack)
                    do (let ((o2 (car operator-stack)))
                         (if (or
                              (and
                               (not (:is-r o1))
                               (<= (:precedence o1) (:precedence o2)))
                              (and
                               (and
                                (:is-r o1)
                                (< (:precedence o1) (:precedence o2)))))
                             (progn
                               (pop operator-stack)
                               (push o2 result-queue))
                             ;; Exit the loop
                             (return))))
              (push o1 operator-stack)

              (skip-empty)
              (get (length next-nonempt))

              (go repeat)))
           (t
            (skip-empty)

            (let* ((*should-unwind* t)
                   (maybe-parsed-expr (parse-expression nil)))
              (unless (null maybe-parsed-expr)
                (push maybe-parsed-expr result-queue)
                (go repeat)))))))

    (loop until (null operator-stack)
          do (if (equal (:name (car operator-stack)) "(")
                 (parsing-error "Mismatching parens")
                 (push (pop operator-stack) result-queue)))

    (let (arg-stack
          (reversed-queue (reverse result-queue)))
      (loop until (null reversed-queue)
            do (let ((curr-tok (pop reversed-queue)))
                 (etypecase curr-tok
                   (ast-value
                    (push curr-tok arg-stack))
                   (operator
                    (unless (find (:name curr-tok) '("(" ")") :test #'equal)
                      (let ((rhs (pop arg-stack))
                            (lhs (pop arg-stack)))
                        (push (make-instance 'ast-function-call
                                             :name (:name curr-tok)
                                             :arguments (list lhs rhs))
                              arg-stack)))))))

      (when (/= 1 (length arg-stack))
        (parsing-error "Mismatched number of arguments for binary operator"))
      (when (and (typep (car arg-stack) 'operator) (equal (:name (car arg-stack)) "("))
        (parsing-error "Mismatching parens"))

      (car arg-stack))))

(def-unwindable-parser parse-expression (&optional (test-for-binary-op t))
  (when test-for-binary-op
    (let* ((*should-unwind* t)
           (maybe-parsed-op (parse-binary-operator)))
      (when maybe-parsed-op
        (return-from parse-expression maybe-parsed-op))))

  (let ((next-char (peek)))
    (cond
      ((numeric-char-p next-char)
       (parse-number))
      ((equal next-char #\*)
       (let ((depth 0))
         (tagbody more-depth
            (when (equal (peek) #\*)
              (get)
              (skip-empty)
              (incf depth)
              (go more-depth)))
         (let ((var-name (parse-ident)))
           (make-instance 'ast-variable-dereference-value
                          :name var-name
                          :depth depth))))
      ((equal next-char #\&)
       (get)
       (skip-empty)
       (let ((var-name (parse-ident)))
         (make-instance 'ast-variable-reference-value
                        :name var-name)))

      ((equal (peek-ident) "true")
       (expect-ident "true")
       (make-instance 'ast-bool :value t))

      ((equal (peek-ident) "false")
       (expect-ident "false")
       (make-instance 'ast-bool :value nil))

      ((not (null (peek-ident)))
       ;; Depending on whether it has parens after it or not,
       ;; it can either be a variable or a function call
       (let* ((next-ident (peek-ident))
              (next-char (peek-nonempty (length next-ident))))
         (case next-char
           (#\( (parse-function-call))
           (otherwise (make-instance 'ast-variable-value :name (parse-ident))))))
      (t
       (parsing-error "Unrecognized expression")))))

(def-unwindable-parser parse-fn-like-define ()
  (let ((f-name (parse-ident)))
    (skip-empty)
    (expect #\()
    (let ((f-args '()))
      (tagbody next-arg
         (skip-empty)
         (unless (equal (peek) #\))
           ;; Parse input variable names
           (push (parse-ident) f-args)
           (skip-empty)
           (let ((next-char (peek)))
             (expect '(#\, #\)))
             (when (equal next-char #\,)
               (go next-arg)))))
      (cons f-name (reverse f-args)))))

(defun parse-fn-like-define-call (expected-name)
  (let ((f-name (parse-ident)))
    ;; Return nothing if the name is different
    (unless (equal f-name expected-name)
      (return-from parse-fn-like-define-call))
    (skip-empty)
    ;; If the next token isn't an opening bracket, it may be a variable of the same name
    (unless (equal (get) #\()
      (return-from parse-fn-like-define-call))
    (let ((f-args '()))
      (tagbody
       next-arg
         (let ((unclosed-parens 0)
               (accumulated-value (make-array '(0) :element-type 'base-char :adjustable t)))
           (loop for curr-char = (get)
                 do (progn
                      (case curr-char
                        ;; Increase the number of unclosed parens and let the paren be added to the accumulator
                        (#\( (incf unclosed-parens))
                        ;; The comma inside an unclosed paren counts as the value inside, not as a separator,
                        ;; so it will be pushed to the accumulator afterwards
                        (#\, (when (zerop unclosed-parens)
                               (progn
                                 ;; Argument ends here, push the accumulator and start processing the next argument
                                 (push accumulated-value f-args)
                                 (go next-arg))))
                        (#\) (if (zerop unclosed-parens)
                                 (progn
                                   ;; Call ends here, push the saved value and exit
                                   (push accumulated-value f-args)
                                   (go last-arg-processed))
                                 ;; Decrease the number of unclosed parens and add it to the accumulator
                                 (decf unclosed-parens)))
                        ((nil) (parsing-error "Reached the end of input while processing a call to a function-like macro ~A" expected-name)))
                      ;; If control reaches here, push the char into the accumulator
                      (vector-push-extend curr-char accumulated-value))))
       last-arg-processed
         ;; Reverse the argument list and return it
         (return-from parse-fn-like-define-call (reverse f-args))))))

(defun for-each-char-until-define-end (fnc)
  (loop for curr-char = (get)
        ;; Break if the end of the file is reached
        until (null curr-char)
        do (progn
             (cond
               ((and (equal curr-char #\\)
                     (equal (peek-nonempty) #\Newline))

                ;; Skip until after the newline
                (loop until (equal curr-char #\Newline)
                      do (setf curr-char (get)))
                (setf curr-char (get)))
               ;; If a newline is not escaped, exit
               ((equal curr-char #\Newline)
                (loop-finish)))
             (funcall fnc curr-char))))

(defun parse-define-directive ()
  (flet ((parse-body ()
           (let ((value (make-array '(0) :element-type 'base-char :adjustable t)))
             (skip-empty)
             (for-each-char-until-define-end
              (lambda (curr-char)
                (vector-push-extend curr-char value)))
             value)))
    (let* ((*should-unwind* t)
           ;; Since a function-like macro definition is similiar to a function call, parse it like one
           (maybe-function-like (parse-fn-like-define)))
      (if maybe-function-like
          (destructuring-bind (name . arg-names) maybe-function-like
            (make-preproc-macro name (parse-body) :args arg-names))
          ;; If it's not a function-like macro, parse the name and the rest of the body
          (make-preproc-macro (parse-ident) (parse-body))))))

(defun insert-instead-string (str-to-operate str-to-ins before-first-char after-last-char)
  (concatenate 'string
               (subseq str-to-operate 0 before-first-char)
               str-to-ins
               (subseq str-to-operate after-last-char)))

(defun insert-instead (str before-first-char after-last-char)
  (symbol-macrolet ((inner-string (:inner-string *context*)))
    (setf inner-string
          (insert-instead-string inner-string str before-first-char after-last-char))))

(defun preprocess-directive ()
  (let ((pos-before-directive (save-position-data)))
    (expect #\#)
    (skip-empty)
    (let* ((dir-name (parse-ident))
           (*skip-chars* '(#\Space)))
      (flet ((parse-expressions ()
               (loop for next-nonempt = (peek-nonempty)
                     until (or (equal next-nonempt #\Newline)
                               (null next-nonempt))
                     for expr = (parse-expression)
                     collect expr)))
        (skip-empty)
        (match dir-name
          ("pragma"
           (parse-expressions)
           nil)
          ("define"
           (let* ((parsed-define (parse-define-directive))
                  (pos-after-define (save-position-data)))
             ;; Cut the define out
             (insert-instead "" pos-before-directive pos-after-define)
             ;; Reset the position
             (load-position-data pos-before-directive)
             ;; Now process the rest with the define applied
             (preprocess-define-directive parsed-define)
             ;; And reset the position again
             (load-position-data pos-before-directive)

             (:inner-string *context*)))
          (otherwise
           (parsing-error "Invalid preprocessor directive ~A found" dir-name)))))))



(defun preprocess-define-directive (parsed-define)
  (with-accessors ((name :name) (args :arguments) (value :value)) parsed-define
    (flet ((try-replacing (&key fn-like-args)
             (if args
               (let* ((before-pos (save-position-data))
                      (maybe-parsed-call-args (parse-fn-like-define-call name))
                      (after-pos (save-position-data)))
                 (when maybe-parsed-call-args
                   (unless (= (length args) (length maybe-parsed-call-args))
                     (parsing-error "Function-like macro ~A called with ~A args, but expects ~A"
                                    name (length maybe-parsed-call-args) (length args)))

                   ;; Replace the call with the unsubstituted value
                   (insert-instead value before-pos after-pos)
                   ;; Restore the position to before the call
                   (load-position-data before-pos)

                   (let ((arg-values (loop for n from 0 below (length args)
                                           collect (cons (nth n args) (nth n maybe-parsed-call-args)))))
                     (let ((starting-position (save-position-data))
                           (value-length (length value)))
                       (loop while (< (- (save-position-data) starting-position) value-length)
                             do (let ((char-at-pos (peek)))
                                  (if (first-character-p char-at-pos)
                                      (let* ((pos-before-ident (save-position-data))
                                             (parsed-ident (parse-ident))
                                             (pos-after-ident (save-position-data))
                                             (maybe-found-arg-value (find parsed-ident arg-values :test #'equal :key #'car)))
                                        (when (and maybe-found-arg-value
                                                   (or (not fn-like-args)
                                                       (not (find parsed-ident fn-like-args :test #'equal))))
                                          (insert-instead (cdr maybe-found-arg-value) pos-before-ident pos-after-ident)
                                          ;; Change the size to account for the different in lengths of
                                          ;; the argument name and value
                                          (decf value-length (- (length (car maybe-found-arg-value))
                                                                (length (cdr maybe-found-arg-value))))
                                          ;; Set the position right after the replaced data
                                          (load-position-data (+ pos-before-ident
                                                                 (length (cdr maybe-found-arg-value))))))
                                      ;; Skip the character
                                      (get))))))))
               (let* ((before-ident-pos (save-position-data))
                      (parsed-ident (parse-ident))
                      (after-ident-pos (save-position-data)))
                 (when (equal parsed-ident name)
                   ;; If either we're not inside a function-like define,
                   ;; or the name is not one of the macro's argument names,
                   ;; replace the identifier with the macro value
                   (if (or (not fn-like-args)
                           (and fn-like-args
                                (not (find parsed-ident fn-like-args :test #'equal))))
                       (insert-instead value before-ident-pos after-ident-pos)))))))
      (loop for curr-char = (peek)
            until (null curr-char)
            do (progn
                 (cond
                   ((equal curr-char #\#)
                    (get)
                    (ematch (parse-ident)
                      ("pragma"
                       ;; Skip to the next line
                       (loop for cr = (get)
                             until (or (equal cr #\Newline)
                                       (null cr))))
                      ("define"
                       (skip-empty)
                       (let* ((*should-unwind* t)
                              (maybe-function-like (parse-fn-like-define)))
                         (if maybe-function-like
                             (destructuring-bind (name . arg-names) maybe-function-like
                               ;; Name is not important here
                               (declare (ignore name))
                               ;; Try replacing the values inside the define body,
                               ;; but not replacing the identifiers that are named like
                               ;; fn-like define arguments
                               (for-each-char-until-define-end
                                (lambda (curr-char)
                                  (when (first-character-p curr-char)
                                    ;; Go back that one character and try replacing the identifier
                                    (back)
                                    (try-replacing :fn-like-args arg-names)))))
                             ;; If it's not a function-like macro, just skip the name
                             (parse-ident))))))
                   ((first-character-p curr-char)
                    ;; If the character is a beginning of an identifier, try replacing it with a macro
                    (try-replacing))
                   (t
                    ;; Skip the charcter
                    (get))))))))

(defun parse-toplevel ()
  (skip-empty)
  (case (peek)
    (#\#
     (preprocess-directive))
    (t
     (parse-function-definition))))

(defun parse-toplevels ()
  (remove-if
   #'null
   (loop until (null (peek))
         collect (progn
                   (skip-empty)
                   (let ((res (parse-toplevel)))
                     (skip-empty)
                     res)))))

(defun parse-file (filename)
  (let* ((file-text
           (with-open-file (stream filename)
             (let ((data (make-string (file-length stream))))
               (read-sequence data stream)
               data)))
         (parsing-context (make-instance 'parsing-context
                                         :string file-text)))
    (let ((*context* parsing-context))
      (parse-toplevels))))
