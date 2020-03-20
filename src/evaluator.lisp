(defpackage lc.evaluator
  (:use :cl :trivia
        :lc.parser :lc.memory :lc.evaluator.intrinsics :lc.evaluator.runtime-function)
  (:export :run :run-with-state :run-with-current-state :with-state
           :evaluator-state :*evaluator-state*
           :runtime-function
           :translate))
(in-package lc.evaluator)



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

(defclass label-instruction (instruction)
  ((name :initarg :name :accessor :name)))

(defmethod print-object ((obj label-instruction) out)
  (print-unreadable-object (obj out)
    (format out "label ~A:"
            (:name obj))))

(defclass exit-instruction (instruction) ())

(defmethod print-object ((obj exit-instruction) out)
  (print-unreadable-object (obj out)
    (format out "exit")))

(defclass jump-instruction (instruction)
  ((label-name :initarg :label-name :accessor :label-name)))

(defmethod print-object ((obj jump-instruction) out)
  (print-unreadable-object (obj out)
    (format out "jump ~A"
            (:label-name obj))))

(defclass jump-if-false-instruction (jump-instruction)
  ((condition-value :initarg :condition-value :accessor :condition-value)))

(defmethod print-object ((obj jump-if-false-instruction) out)
  (print-unreadable-object (obj out)
    (format out "jump-if-false ~A, ~A"
            (:label-name obj) (:condition-value obj))))

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

(defun get-translation-local-variable-type (name)
  (gethash name (car (:translation-locals-types-stack *evaluator-state*))))

(defun set-translation-local-variable-type (name type)
  (symbol-macrolet ((var-in-hash (gethash name (car (:translation-locals-types-stack *evaluator-state*)))))
    (setf var-in-hash type)))

(defun make-save-into-instruction (name type)
  (set-translation-local-variable-type name type)
  (make-instance 'save-into-instruction :name name :type type))

(defclass call-instruction (instruction)
  ((name :initarg :name
         :accessor :name)))

(defmethod print-object ((obj call-instruction) out)
  (print-unreadable-object (obj out)
    (format out "call ~A" (:name obj))))

(defmethod type-of-ast ((ast ast-variable-value))
  (let* ((name (:name ast))
         (types (car (:translation-locals-types-stack *evaluator-state*)))
         (var-type (gethash name types)))
    (when (null var-type)
      (error "Variable ~A undefined" name))
    var-type))

(defmethod type-of-ast ((ast ast-function-call))
  (let ((defined-func (get-defined-func (:name ast) (:arguments ast))))
    (:return-type defined-func)))

(defgeneric evaluate (value))

(defgeneric evaluate-expr (value))

(defmethod evaluate-expr ((value ast-float))
  (with-accessors ((float-val :value)) value
    (let ((mem-type (type-of-ast value)))
      (make-instance 'memory-value
                     :value-type mem-type
                     :value float-val))))

(defmethod evaluate-expr ((value ast-integer))
  (with-accessors ((int-val :value)) value
    (let ((mem-type (type-of-ast value)))
      (make-instance 'memory-value
                     :value-type mem-type
                     :value int-val))))

(defmethod evaluate-expr ((value ast-bool))
  (with-accessors ((val :value)) value
    (if val +memory-true+ +memory-false+)))

(defmethod evaluate-expr ((value ast-variable-value))
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

(defmethod evaluate ((func-decl ast-function-declaration))
  (with-accessors ((func-name :name) (ret-type :return-type) (args :arguments)) func-decl
    (symbol-macrolet ((func-in-hash (gethash func-name (:functions *evaluator-state*))))
      (let* ((runtime-ret-t (ast-to-memory-type ret-type))
             (runtime-args (map 'list #'ast-to-memory-arg args))
             (runtime-f-decl
               (make-instance 'runtime-function
                              :name func-name
                              :return-type runtime-ret-t
                              :arguments runtime-args)))
        (when (not (null func-in-hash))
          (warn "Redefining function '~A'" func-name))

        ;; Store the function declaration
        (setf func-in-hash runtime-f-decl)))))

(defmethod evaluate ((func-def ast-function-definition))
  (with-accessors ((f-decl :function-declaration) (body :body)) func-def
    (let ((runtime-f-decl (evaluate f-decl)))
      (with-accessors ((r-body :body) (r-args :arguments) (r-label-table :label-table)) runtime-f-decl
        (setf r-body (make-array (length r-body) ;; Make it at least as big as the emount of ast entries
                                 :adjustable t
                                 :fill-pointer 0))

        ;; Create a new locals types frame
        (push (make-hash-table :test #'equal) (:translation-locals-types-stack *evaluator-state*))

        ;; Push args
        (loop for arg in r-args
              do (vector-push-extend (make-save-into-instruction (car arg) (cdr arg)) r-body))

        ;; Put save-sp after the arguments, so when the stack is saved,
        ;; things can be written over old arguments
        (vector-push-extend (make-instance 'save-sp-instruction) r-body)

        ;; Set the dynamic variable for the function currently in translation
        (let ((instr-number (length r-body)))
          ;; Compile instructions and put them into the runtime function
          (loop for instr in (alexandria:flatten (mapcar #'translate body))
                do (progn
                     (match instr
                       ;; For label instructions, save their number. Since it's
                       ((class label-instruction)
                        (setf (gethash (:name instr) r-label-table) instr-number)))
                     (vector-push-extend instr r-body)
                     (incf instr-number))))


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
         (tmp-local-name (format nil "$tmp_~A" (hash-table-count (car (:translation-locals-types-stack *evaluator-state*)))))
         ;; The function is guaranteed to be defined here, since it's checked in (translate call)
         (defined-func (get-defined-func (:name call) (:arguments call)))
         (save-into-tmp-instr (make-save-into-instruction tmp-local-name (:return-type defined-func))))
    (cons
     ;; First, return values to generate to load the data
     (append call-instrs (list save-into-tmp-instr))
     ;; Second, return the what to use to actually load that data
     (make-instance 'ast-variable-value :name tmp-local-name))))

(defmethod translate-expr ((int ast-integer))
  (cons nil int))

(defmethod translate-expr ((float ast-float))
  (cons nil float))

(defmethod translate-expr ((val ast-variable-value))
  (cons nil val))

(defmethod translate-expr ((val ast-bool))
  (cons nil val))

(defmethod translate ((ret ast-return))
  (destructuring-bind (expr-generated-instrs . expr-load-value) (translate-expr (:value ret))
    (append
     (list
      (make-instance 'comment-instruction :comment "return begins here"))
     ;; Insert instructions generated for the returning expression
     expr-generated-instrs
     (list
      ;; Reset the stack pointer
      (make-instance 'restore-sp-instruction)
      ;; Push the returned expression onto the stack
      (make-instance 'push-instruction :pushed-value expr-load-value)
      ;; Exit the function
      (make-instance 'exit-instruction)))))

(defun get-defined-func (name &optional (args nil args-supplied))
  (let ((defined-func (gethash name (:functions *evaluator-state*))))
    ;; If the function cannot be found, try searching for an intrinsic function
    (unless defined-func
      (setf
       defined-func
       (if args-supplied
           ;; When there are arguments provided, look up an intrinsic dispatcher and try getting an intrinsic there
           (dispatch-intrinsic name args)
           ;; If there are no arguments provided, the call is probably from an instruction, which doesn't know
           ;; about the number of arguments. Look up the intrinsic table directly, since the name should already
           ;; be the one needed
           (intrinsic-from-actual-name name)))
      ;; If it's still undefined, there was no intrinsic
      (unless defined-func
        (error "Function ~A is not declared or defined" name)))
    defined-func))

(defmethod translate ((func-call ast-function-call))
  (with-accessors ((args :arguments)) func-call
    (let ((defined-func (get-defined-func (:name func-call) (:arguments func-call))))
      ;; Use the name of the looked-up function, since it may be an intrinsic
      (with-accessors ((def-args :arguments) (name :name)) defined-func
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

                           (destructuring-bind (arg-generation-instrs . arg-loading-place) (translate-expr arg-at-n)
                             ;; Push the actual putting-argument-onto-the-stack with the actual arg loading place first
                             (push (make-instance 'push-instruction :pushed-value arg-loading-place) push-args-instructions)

                             ;; Push arg generation instruction backwards, since push adds them to the beginning.
                             ;; Also, push them after the the actual loading instruction, because push works backwards
                             (loop for instr in (reverse arg-generation-instrs)
                                   do (push instr push-args-instructions)))))))
            (let ((calling-instruction (list (make-instance 'call-instruction :name name))))
              (append push-args-instructions
                      calling-instruction))))))))

(define-condition translation-error (error)
  ((message :initarg :message)
   (ast-object :initarg :ast-object))
  (:report (lambda (condition stream)
             (with-slots (message ast) condition
               (format stream message)
               (format stream "~2%In SLIME/Sly press C to inspect condition. The AST-OBJECT slot contains the AST object that caused the error.")))))

(define-condition variable-already-declared (translation-error) ())

(define-condition variable-not-declared (translation-error) ())

(defun translation-error (class ast-object &rest format-args)
  (unless (null format-args)
    (push nil format-args))
  (error class
         :message (apply #'format format-args)
         :ast-object ast-object))

(defmethod translate ((var-decl ast-variable-declaration))
  (let ((name (:name var-decl)))
    (when (get-translation-local-variable-type name)
      (translation-error 'variable-already-declared var-decl "Variable ~A already declared" name))

    (set-translation-local-variable-type name (ast-to-memory-type (:decl-type var-decl)))
    ;; Don't return anything
    nil))

(defmethod translate ((var-def ast-variable-definition))
  ;; Handle the case that a variable might already be defined
  (translate (:var-decl var-def))

  (destructuring-bind (val-generation-instrs . val-loading-place) (translate-expr (:value var-def))
    (append
     ;; Return val generation instruction first
     val-generation-instrs
     (list
      ;; Push the value of the variable onto the stack
      (make-instance 'push-instruction :pushed-value val-loading-place)
      (let ((decl (:var-decl var-def)))
        ;; Push the saving into the variable onto the stack. This will also declare the variable.
        (make-save-into-instruction (:name decl) (ast-to-memory-type (:decl-type decl))))))))

(defmethod translate ((var-asgn ast-variable-assignment))
  (with-accessors ((name :name) (value :value)) var-asgn
    (let ((var-type (get-translation-local-variable-type name)))
      (unless var-type
        (translation-error 'variable-not-declared var-asgn "Variable ~A is not declared, but assigned to" name))
      (unless (memory-type-equal var-type (type-of-ast value)))

      (destructuring-bind (val-generation-instrs . val-loading-place) (translate-expr value)
        (append
         ;; Return val generation instruction first
         val-generation-instrs
         (list
          ;; Push the value of the variable onto the stack
          (make-instance 'push-instruction :pushed-value val-loading-place)
          ;; Push the saving into the variable onto the stack.
          (make-save-into-instruction name var-type)))))))

(defmethod translate ((if-else ast-if-else))
  (with-accessors ((condition :condition) (if-body :if-body) (else-body :else-body)) if-else
    (destructuring-bind (cond-val-generation-instrs . cond-val-loading-place) (translate-expr condition)
      (let* ((current-number-of-labels (:global-label-count *evaluator-state*))
             (after-if-label-name (format nil "$after_if_~A" current-number-of-labels))
             (after-if-label (make-instance 'label-instruction :name after-if-label-name))
             ;; The label after the else, that should be jumped to when `if-body' finishes
             (after-else-label-name (format nil "$after_else_~A" current-number-of-labels))
             (after-else-label (make-instance 'label-instruction :name after-else-label-name)))
        (incf (:global-label-count *evaluator-state*))

        (append
         ;; Generate condition value
         cond-val-generation-instrs
         ;; jump-if-false to the end of the if
         (list
          (make-instance 'jump-if-false-instruction :label-name after-if-label-name :condition-value cond-val-loading-place))
         ;; Generate `if-body'
         (alexandria:flatten (mapcar #'translate if-body))
         ;; Jump to after the else if the if the main branch is executed
         (when else-body
           (list (make-instance 'jump-instruction :label-name after-else-label-name)))
         (list
          ;; Put in the after-if-label
          after-if-label)
         ;; Generate the `else-body' if it exists
         (when else-body
           (append
            ;; Generate the body
            (alexandria:flatten (mapcar #'translate else-body))
            ;; Generate the after-else label
            (list after-else-label))))))))

(defclass evaluator-state ()
  ((stack :initform (make-array 16 :adjustable t :initial-element nil) :accessor :stack)
   (stack-pointer :initform 0 :accessor :stack-pointer)
   (saved-stack-pointers :initform nil :accessor :saved-stack-pointers)
   (locals-stack :initform nil :accessor :locals-stack)

   (functions :initform (make-hash-table :test #'equal) :accessor :functions)

   (translation-locals-types-stack :initform nil :accessor :translation-locals-types-stack)

   (global-label-count :initform 0 :accessor :global-label-count)))

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

(defun push-onto-stack (value)
  (with-accessors ((sp :stack-pointer) (stack :stack)) *evaluator-state*
    (when (= sp (length stack))
      (adjust-array stack (* (length stack) 2) :initial-element nil))

    (setf (aref stack sp) (evaluate-expr value))
    (incf sp)))

(defmethod evaluate-instr ((instr push-instruction))
  (push-onto-stack (:pushed-value instr)))

(defmethod evaluate-instr ((instr call-instruction))
  (let* ((runtime-f (get-defined-func (:name instr)))
         (body (:body runtime-f)))
    (push (make-hash-table :test 'equal) (:locals-stack *evaluator-state*))

    (ematch body
      ;; If the body is a vector of instructions, pass evaluation to `evaluate-function'
      ((type vector)
       (evaluate-function runtime-f))
      ((type function)
       (let (args)
         (loop for n from 1 to (length (:arguments runtime-f))
               do (push (pop-from-stack) args))
         ;; Save SP before calling the function
         (save-sp)
         ;; Call the intrinsic with the poppped argument list reversed, to they're in the correct order
         (let ((result (apply body (reverse args))))
           ;; After the call, restore SP and push the result onto the stack
           (restore-sp)
           (push-onto-stack result)))))

    (pop (:locals-stack *evaluator-state*))))

(defmethod evaluate-function ((func runtime-function))
  (with-accessors ((func-name :name) (func-instrs :body) (label-table :label-table)) func
    (let ((n 0))
      (loop while (< n (length func-instrs))
            for current-instr = (elt func-instrs n)
            do (ematch current-instr
                 ((class jump-if-false-instruction)
                  (let* ((label-name (:label-name current-instr))
                         (found-label-num (gethash label-name label-table)))
                    ;; Maybe this should be moved to compile-time/translation time somehow
                    (unless found-label-num
                      (error "Label ~A not found in function ~A" label-name func-name))
                    ;; Set N to the new position determined by `evaluate-jump-if-false'
                    (setf n (evaluate-jump-if-false current-instr n found-label-num))))
                 ((class jump-instruction)
                  (let* ((label-name (:label-name current-instr))
                         (found-label-num (gethash label-name label-table)))
                    (unless found-label-num
                      (error "Label ~A not found in function ~A" label-name func-name))
                    ;; Set N to the label position unconditionally
                    (setf n found-label-num)))
                 ((class exit-instruction)
                  ;; Stop evaluation after this. If the function reaches its end, this is not required
                  (loop-finish))
                 ((class instruction)
                  ;; Evaluate the instruction and increment the next instruction number
                  (evaluate-instr current-instr)
                  (incf n)))))))

(defmethod evaluate-instr ((instr save-into-instruction))
  (setf (gethash (:name instr)
                 (car (:locals-stack *evaluator-state*)))
        (pop-from-stack)))

;; Deliberately left empty
(defmethod evaluate-instr ((instr label-instruction)))

(defun evaluate-jump-if-false (jump-instr current-pos label-pos)
  (let ((cond-result (evaluate-expr (:condition-value jump-instr))))
    (unless (memory-type-equal (:value-type cond-result) +bool-t+)
      (error "~A in an evaluate-jump-false was not a boolean" cond-result))
    (if (:value cond-result)
        ;; If result is true, do not jump, just proceed to the next instruction
        (1+ current-pos)
        ;; If result is false, jump to the label instruction
        label-pos)))

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
  (loop for instr across (:body fnc)
        do (print instr)))

(defun test ()
  (with-state (make-instance 'lc.evaluator:evaluator-state)
    (loop for f in (parse-file "test.c")
          do (evaluate f))

    ;;(disasm-runtime-function (gethash "test" (:functions lc.evaluator::*evaluator-state*)))

    (run-with-current-state
     (translate
      (let ((lc.parser::*context* (make-instance 'lc.parser::parsing-context :string "test_add(21,9)")))
        (lc.parser::parse-function-call))))

    *evaluator-state*))
