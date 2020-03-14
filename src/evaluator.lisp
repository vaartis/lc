(defpackage lc.evaluator
  (:use :cl :lc.parser :lc.memory)
  (:export :run :run-with-state :run-with-current-state :with-state
           :evaluator-state :*evaluator-state*
           :translate))
(in-package lc.evaluator)

(defclass runtime-function ()
  ((name :initarg :name :accessor :name)
   (arguments :initarg :arguments :accessor :arguments)
   (body :initarg :body :accessor :body)
   (return-type :initarg :return-type :accessor :return-type
                :type memory-type))
  (:default-initargs :body nil))

(defclass instruction () ())

(defclass push-instruction (instruction)
  ((pushed-value :initarg :pushed-value
                 :accessor :pushed-value)))

(defmethod print-object ((obj push-instruction) out)
  (print-unreadable-object (obj out)
    (format out "push ~A"
            (:pushed-value obj))))

(defclass comment-instruction (instruction)
  ((comment :initarg :comment
            :accessor :comment)))

(defmethod print-object ((obj comment-instruction) out)
  (print-unreadable-object (obj out)
    (format out "; ~A"
            (:comment obj))))

(defclass save-sp-instruction (instruction) ())

(defmethod print-object ((obj save-sp-instruction) out)
  (print-unreadable-object (obj out)
    (format out "save-sp")))

(defclass restore-sp-instruction (instruction) ())

(defmethod print-object ((obj restore-sp-instruction) out)
  (print-unreadable-object (obj out)
    (format out "restore-sp")))

(defclass save-into-instruction (instruction)
  ((name :initarg :name
         :accessor :name)
   (type :initarg :type
         :accessor :type)))

(defmethod print-object ((obj save-into-instruction) out)
  (print-unreadable-object (obj out)
    (format out "save-into ~A" (:name obj))))

(defun make-save-into-instruction (name type)
  (symbol-macrolet ((var-in-hash (gethash name (car (:translation-locals-types-stack *evaluator-state*)))))
    (setf var-in-hash type)
    (make-instance 'save-into-instruction :name name :type type)))

(defclass call-instruction (instruction)
  ((name :initarg :name
         :accessor :name)))

(defmethod print-object ((obj call-instruction) out)
  (print-unreadable-object (obj out)
    (format out "call ~A" (:name obj))))

(defmethod type-of-ast ((ast ast-integer))
  (get-standard-simple-type "int"))

(defmethod type-of-ast ((ast ast-float))
  (get-standard-simple-type "float"))

(defmethod type-of-ast ((ast ast-variable-value))
  (let* ((name (:name ast))
         (types (car (:translation-locals-types-stack *evaluator-state*)))
         (var-type (gethash name types)))
    (when (null var-type)
      (error "Variable ~A undefined" name))
    var-type))

(defgeneric evaluate (value))

(defgeneric evaluate-value (value))

(defmethod evaluate-value ((value ast-float))
  (with-accessors ((float-val :value)) value
    (let ((mem-type (type-of-ast value)))
      (make-instance 'memory-value
                     :value-type mem-type
                     :value float-val))))

(defmethod evaluate-value ((value ast-integer))
  (with-accessors ((int-val :value)) value
    (let ((mem-type (type-of-ast value)))
      (make-instance 'memory-value
                     :value-type mem-type
                     :value int-val))))

(defmethod evaluate-value ((value ast-variable-value))
  (let* ((name (:name value))
         (locals (car (:locals-stack *evaluator-state*)))
         (var-value (gethash name locals)))
    (when (null var-value)
      (error "Variable ~A wasn't defined at runtime" name))

    var-value))

(defmethod ast-to-memory-type ((ast-tp ast-simple-type))
  (with-accessors ((name :name)) ast-tp
    (let ((corr-mem-type (get-standard-simple-type name)))
      (when (null corr-mem-type)
        (error "Type '~A' not found in standard simple types" name))
      corr-mem-type)))

(defmethod ast-to-memory-type ((ast-tp ast-pointer-type))
  (with-accessors ((to-tp :to-type) (qualifs :qualifiers)) ast-tp
    (make-instance 'pointer-memory-type
                   :qualifiers qualifs
                   :pointer-to (ast-to-memory-type to-tp))))

(defun ast-to-memory-arg (arg)
  (destructuring-bind (name . tp) arg
    (cons name (ast-to-memory-type tp))))

(defun define-runtime-function (func-name ret-type args)
  (symbol-macrolet ((func-in-hash (gethash func-name (:functions *evaluator-state*))))

    (let* ((runtime-ret-t (ast-to-memory-type ret-type))
           (runtime-args (map 'list #'ast-to-memory-arg args))
           (runtime-f-decl
             (make-instance 'runtime-function
                            :return-type runtime-ret-t
                            :arguments runtime-args
                            :name func-name)))
      (when (not (null func-in-hash))
        (warn "Redefining function '~A'" func-name))

      ;; Store the function declaration
      (setf func-in-hash runtime-f-decl)

      runtime-f-decl)))

(defmethod evaluate ((func-decl ast-function-declaration))
  (with-accessors ((func-name :name) (ret-type :return-type) (args :arguments)) func-decl
    (define-runtime-function func-name ret-type args)))

(defmethod evaluate ((func-def ast-function-definition))
  (with-accessors ((f-decl :function-declaration) (body :body)) func-def
    (let ((runtime-f-decl (evaluate f-decl)))
      (with-accessors ((r-body :body) (r-args :arguments)) runtime-f-decl

        ;; Create a new locals types frame
        (push (make-hash-table :test #'equal) (:translation-locals-types-stack *evaluator-state*))

        ;; Push args
        (loop for arg in r-args
              do (push (make-save-into-instruction (car arg) (cdr arg)) r-body))

        ;; Put save-sp after the arguments, so when the stack is saved,
        ;; things can be written over old arguments
        (push (make-instance 'save-sp-instruction) r-body)

        ;; Compile instructions and put them into the runtime function
        (loop for instr in (alexandria:flatten (map 'list #'translate body))
              do (push instr r-body))

        (setf r-body (reverse r-body))

        ;; Pop the locals types frame
        (pop (:translation-locals-types-stack *evaluator-state*))

        ;; Return the declaration/definition (now that it has a body)
        runtime-f-decl))))

(defgeneric translate-expr (expr))

(defmethod translate-expr ((call ast-function-call))
  ;; First, translate the instructions to call the function.
  ;; After it's called, the result will be on the stack
  (let* ((call-instrs (translate call))
         ;; A name for the temporary local to store data in, adding $ to it should make it pretty much uncollidable
         (tmp-local-name (format nil "$tmp_~A" (length (:locals-stack *evaluator-state*))))
         ;; The function is guaranteed to be defined here, since it's checked in (translate call)
         (defined-func (gethash (:name call) (:functions *evaluator-state*)))
         (save-into-tmp-instr (make-save-into-instruction tmp-local-name (:return-type defined-func))))
    (cons
     ;; First, return values to generate to load the data
     (concatenate 'list call-instrs (list save-into-tmp-instr))
     ;; Second, return the what to use to actually load that data
     (make-instance 'ast-variable-value :name tmp-local-name))))

(defmethod translate-expr ((int ast-integer))
  (cons nil int))

(defmethod translate-expr ((float ast-float))
  (cons nil float))

(defmethod translate-expr ((val ast-variable-value))
  (cons nil val))

(defmethod translate ((ret ast-return))
  (destructuring-bind (expr-generated-instrs . expr-load-value) (translate-expr (:value ret))
    (concatenate
     'list
     (list
      (make-instance 'comment-instruction :comment "return begins here"))
     ;; Insert instructions generated for the returning expression
     expr-generated-instrs
     (list
      ;; Reset the stack pointer
      (make-instance 'restore-sp-instruction)
      ;; Push the returned expression onto the stack
      (make-instance 'push-instruction :pushed-value expr-load-value)))))

(defmethod translate ((func-call ast-function-call))
  (with-accessors ((name :name) (args :arguments)) func-call
    (let ((defined-func (gethash name (:functions *evaluator-state*))))
      (unless defined-func
        (error "Function ~A is not declared or defined" name))

      (with-accessors ((def-args :arguments) (body :body) (def-type :return-type)) defined-func
        ;;(unless body
        ;;  (error "Function ~A is declared, but not defined" name))

        (let ((def-args-len (length def-args))
              (args-len (length args)))
          (unless (= def-args-len args-len)
            (error "Wrong number of arguments for ~A: expected ~A, but got ~A"
                   name def-args args))

          (let (push-args-instructions)
            (unless (zerop def-args-len)
              (loop for arg-num from 0 to (1- def-args-len)
                do (let ((def-arg-at-n (nth arg-num def-args))
                         (arg-at-n (nth arg-num args)))
                     (destructuring-bind (def-arg-at-n-name . def-arg-at-n-type) def-arg-at-n
                       (let ((mem-type-of-ast (type-of-ast arg-at-n)))
                         (unless (memory-type-equal def-arg-at-n-type mem-type-of-ast)
                           (error "Wrong type for argument #~A (~A) of call to ~A: expected ~A, but got ~A"
                                  arg-num def-arg-at-n-name name (:type-name def-arg-at-n-type) (:type-name mem-type-of-ast))))

                       (push (make-instance 'push-instruction :pushed-value arg-at-n) push-args-instructions)))))
            (let ((calling-instruction (list (make-instance 'call-instruction :name name))))
              (concatenate 'list
                           push-args-instructions
                           calling-instruction))))))))

(defclass evaluator-state ()
  ((stack :initform (make-array 16 :adjustable t :initial-element nil) :accessor :stack)
   (stack-pointer :initform 0 :accessor :stack-pointer)
   (saved-stack-pointers :initform nil :accessor :saved-stack-pointers)
   (locals-stack :initform nil :accessor :locals-stack)

   (functions :initform (make-hash-table :test #'equal) :accessor :functions)

   (translation-locals-types-stack :initform nil :accessor :translation-locals-types-stack)))

(defvar *evaluator-state*)

(defun pop-from-stack ()
  (symbol-macrolet ((sp (:stack-pointer *evaluator-state*)))
    (when (= sp 0)
      (error "Nothing to pop from stack (stack empty)"))

    ;; Decrement the stack pointer and return the value that is in the decremented position.
    ;; This returns the current value and makes it the one to be written on top of on the next push.
    (decf sp)
    (aref (:stack *evaluator-state*) (:stack-pointer *evaluator-state*))))

(defun save-sp ()
  (push (:stack-pointer *evaluator-state*) (:saved-stack-pointers *evaluator-state*)))

(defun restore-sp ()
  (with-accessors ((saved-sps :saved-stack-pointers) (sp :stack-pointer)) *evaluator-state*
    (let ((saved-sp (pop saved-sps)))
      (if (null saved-sp)
        (error "No saved stack pointer"))
      (setf sp saved-sp))))

(defgeneric evaluate-instr (instruction))

(defmethod evaluate-instr ((instr comment-instruction)))

(defmethod evaluate-instr ((instr save-sp-instruction))
  (save-sp))

(defmethod evaluate-instr ((instr restore-sp-instruction))
  (restore-sp))

(defmethod evaluate-instr ((instr push-instruction))
  (with-accessors ((sp :stack-pointer) (stack :stack)) *evaluator-state*
    (when (= sp (length stack))
      (adjust-array stack (* (length stack) 2) :initial-element nil))
    (setf (aref stack sp) (evaluate-value (:pushed-value instr)))
    (incf sp)))

(defmethod evaluate-instr ((instr call-instruction))
  (let* ((f-name (:name instr))
         (runtime-f (gethash f-name (:functions *evaluator-state*))))
    (when (null runtime-f)
      (error "Undefined runtime function: ~A" f-name))

    (push (make-hash-table :test 'equal) (:locals-stack *evaluator-state*))

    (loop for instr in (:body runtime-f)
          do (evaluate-instr instr))

    (pop (:locals-stack *evaluator-state*))))

(defmethod evaluate-instr ((instr save-into-instruction))
  (setf (gethash (:name instr)
                 (car (:locals-stack *evaluator-state*)))
        (pop-from-stack)))

(defmacro with-state (state &body rest)
  `(let ((*evaluator-state* ,state))
     ,@rest))

(defun run-with-current-state (instructions)
  (loop for instr in instructions
          do (evaluate-instr instr)))

(defmacro run-with-state (state instructions)
  `(with-state ,state
     (run-with-current-state ,instructions)))

(defmacro run (instructions)
  `(run-with-state (make-instance 'evaluator-state) ,instructions))


(defun disasm-runtime-function (fnc)
  (loop for instr in (:body fnc)
        do (print instr)))
