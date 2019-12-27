(defpackage lc.parser
  (:use :cl)
  (:shadow :get :symbol)
  (:export :parse-file :parse-type :parse-function-call
           :parsing-error))
(in-package lc.parser)

(defclass parsing-context ()
  ((inner-string :initarg :string)
   (position :initarg :position)
   (symbol :initarg :symbol)
   (line :initarg :line))
  (:default-initargs :position 0 :symbol 0 :line 1))

(defvar *context*)

(defun get (&optional (advance-by 1))
  "Get the next character and advance the position"
  (let ((val (peek)))
    (with-slots (position symbol line) *context*
      (incf position advance-by)

      (if (equal val #\Newline)
          (progn
            (setf symbol 0)
            (incf line))
          (incf symbol))

      val)))

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
                 (not (find current-char +skip-chars+)))
          return current-char))

(defun peek-ident ()
  "Peek the next identifier or anything that can be considered one (such as keywords)"
  (let ((collected-word
          (loop for skipped-chars = 0 then (1+ skipped-chars)
                for current-char = (peek skipped-chars)
                while (and (not (null current-char))
                           (first-character-p current-char))
                collect current-char)))
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
                                    (char= ch #\.))
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
             (standard-char (char= char expected))
             (cons (some (lambda (ch) (equal char ch)) expected)))))
    (if (not expect-result)
        (parsing-error "Expected ~A, but found '~A'"
                       (typecase expected
                         (standard-char (format nil "'~A'" expected))
                         (cons (format nil "~{'~A'~^ or ~}" expected)))
                       char))))

(define-condition parsing-error (error)
  ((message :initarg :message :reader message)
   (symbol :initarg :symbol :reader symbol)
   (line :initarg :line :reader line))
  (:report (lambda (condition stream)
             (with-slots (message symbol line) condition
               (format stream "~A at line ~A, symbol ~A~%" message line symbol)
               (let* ((line-specifier (format nil "~A| " line))
                      (line-specifier-length (length line-specifier)))
                 (format stream "~A~A~%" line-specifier (get-line-at line))
                 ;; Write a ^ showing the position, skipping the first
                 (loop repeat (1- (+ line-specifier-length symbol)) do (write-char #\Space stream))
                 (write-char #\^ stream))))))

(defun get-line-at (line-num)
  (with-slots (inner-string) *context*
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
           :symbol symbol
           :line line)))

(defvar +skip-chars+ '(#\Newline #\Space))
(defun skip-empty ()
  "Skips empty tokens like spaces and newlines"
  (loop for ch = (peek)
        while (find ch +skip-chars+)
        do (get)))

(defun first-character-p (char)
  (or (alpha-char-p char) (equal char #\_)))

(defun ident-character-p (char)
  (when (null char)
    (return-from ident-character-p nil))

  (or (alphanumericp char) (equal char #\_)))

(defvar +numeric-chars+ (coerce "1234567890" 'list))
(defun numeric-char-p (char)
  (find char +numeric-chars+))

(defun parse-ident ()
  (let ((first-char (get)))

    (unless (alpha-char-p first-char)
      (parsing-error "Expected the first identifier character to be an alpha character, but got '~A'" first-char))

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

(defclass ast-pointer-assignment (ast-variable-assignment)
  ;; TODO: figure something better out if this doesn't work well
  ((depth :initarg :depth :accessor :depth)))

(defclass ast-value (ast) ())

(defclass ast-integer (ast-value)
  ((value :initarg :value :accessor :value)))

(defclass ast-float (ast-value)
  ((value :initarg :value :accessor :value)))

(defclass ast-variable-value (ast-value)
  ((name :initarg :name :accessor :name)))

(defclass ast-variable-reference-value (ast-variable-value) ())

(defclass ast-variable-dereference-value (ast-variable-value)
  ((depth :initarg :depth :accessor :depth)))

(defclass ast-function-call (ast-value)
  ((name :initarg :name :accessor :name)
   (arguments :initarg :arguments :accessor :arguments)))

(eval-when (:load-toplevel :execute)
  (labels ((all-subclasses (class-name)
             (let* ((class (find-class class-name))
                    (subclasses (closer-mop:class-direct-subclasses class)))
               (alexandria:flatten
                (map 'list (lambda (class)
                             (let ((subclasses
                                     (all-subclasses (class-name class))))
                               (if (null subclasses) class subclasses)))
                     subclasses)))))
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

(defvar +simple-type-main-words+ '("void" "char" "int" "float" "double"))
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
  (let ((unwind-position-name (gensym "unwind-position")))
    `(defun ,name (,@params)
       (if *should-unwind*
           (let ((,unwind-position-name (slot-value *context* 'position)))
             (handler-case
                 (progn ,@body)
               (parsing-error ()
                 (setf (slot-value *context* 'position) ,unwind-position-name)
                 nil)))
           (progn ,@body)))))

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
          (if (not (char= (peek) #\)))
              (let ((arguments '()))
                (tagbody parse-arg
                   (let ((type (parse-type)))
                     (skip-empty)
                     (let ((name (parse-ident)))
                       (push (cons name type) arguments)

                       (skip-empty)
                       (when (char= (peek) #\Comma)
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
            (#\{ (let ((f-body (parse-function-body)))
                   (make-instance 'ast-function-definition
                                  :function-declaration f-decl
                                  :body f-body)))
            (otherwise (parsing-error "Expected '{' or ';' after the '~A' function declaration, but found ~A" f-name current-char))))))))

(defun parse-function-body ()
  (expect #\{)

  (let ((statements '()))
    ;; Loop while next char is not a closing bracket,
    ;; pushing statements into the statements list
    (loop while (progn
                  (skip-empty)
                  (not (char= (peek) #\})))
          do (push (parse-statement) statements))
    ;; Skip the } at the end
    (expect #\})

    ;; Revers the statement list since push works backwards
    (reverse statements)))

(defun parse-statement ()
  (let* ((next-ident (peek-ident))
         (statement
           (cond
             ((string= next-ident "return")
              (parse-ident)
              (skip-empty)
              (make-instance 'ast-return
                             :value (parse-expression)))
             ((or
               (char= (peek) #\*)
               (let ((ident-length (length next-ident)))
                 (let ((char-after-ident (peek-nonempty ident-length)))
                   ;; TODO: handle *= /= etc.
                   (char= char-after-ident #\=))))
              (parse-variable-assignment))
             ;; As a last case, try parsing as a variable definition
             (t
              (let* ((*should-unwind* t)
                     (maybe-parsed-variable (parse-variable-definition)))
                (if maybe-parsed-variable
                    maybe-parsed-variable
                    (parse-expression))))
             ;; (t (parsing-error "Expected an identifier at the beinning of the statement, but found ~A" next-ident))
             )))
    (skip-empty)
    (expect #\;)

    statement))

(defun parse-variable-assignment ()
  (let ((depth 0))
    (when (char= (peek) #\*)
      (tagbody more-depth
         (when (char= (peek) #\*)
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

(defun parse-expression ()
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

      ((not (null (peek-ident)))
       ;; Depending on whether it has parens after it or not,
       ;; it can either be a variable or a function call
       (let* ((next-ident (peek-ident))
              (next-char (peek-nonempty (length next-ident))))
         (case next-char
           (#\( (parse-function-call))
           (otherwise (make-instance 'ast-variable-value :name (parse-ident)))))))))

(defun parse-toplevel ()
  (skip-empty)
  (parse-function-definition))

(defun parse-file (filename)
  (let* ((file-text
           (with-open-file (stream filename)
             (let ((data (make-string (file-length stream))))
               (read-sequence data stream)
               data)))
         (parsing-context (make-instance 'parsing-context
                                         :string file-text)))
    (let ((*context* parsing-context))
      (parse-toplevel))))
