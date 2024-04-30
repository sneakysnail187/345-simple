#lang racket
(require "classParser.rkt")

(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
    (lambda (statement)
        (not (null? (cddr statement)))
    )
)

(define exists-operand3?
    (lambda (statement)
        (not (null? (cdddr statement)))
    )
)

(define interpret
    (lambda (file class-name)
        (call/cc
            (lambda (return)
                (interpret-statement-list 
                    (parser file) 
                    (newenvironment) 
                    class-name 
                    return
                    (lambda (env) (myerror "Break outside loop"))
                    (lambda (env) (myerror "Continue used outside loop"))
                    (lambda (v env) (myerror "Uncaught Exception"))
                )
            )
        )
    )
)

; interprets the list of statements
(define interpret-statement-list
    (lambda (statement-list environment class-name return break continue throw)
        (if (null? statement-list)
            (evaluate-main-class class-name environment return break continue throw)
            (interpret-statement-list (remaining-lines statement-list) (interpret-statement (first-line statement-list) environment return break continue throw) class-name return break continue throw)
        )
    )
)

; Abstraction for interpret-statement-list
(define remaining-lines cdr)
(define first-line car)

; interprets singular statements
(define interpret-statement
    (lambda (statement environment return break continue throw)
        (cond
            [(eq? 'return (statement-type statement)) (interpret-return statement environment return throw)]
            [(eq? 'var (statement-type statement)) (interpret-declare statement environment return break continue throw)]
            [(eq? '= (statement-type statement)) (interpret-assign statement environment throw)]
            [(eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw)]
            [(eq? 'while (statement-type statement)) (interpret-while statement environment return throw)]
            [(eq? 'continue (statement-type statement)) (continue environment)]
            [(eq? 'break (statement-type statement)) (break environment)]
            [(eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw)]
            [(eq? 'throw (statement-type statement)) (interpret-throw statement environment throw)]
            [(eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw)]
            [(eq? 'function (statement-type statement)) (interpret-function (function-block statement) environment return break continue throw)]
            [(eq? 'static-function (statement-type statement)) (interpret-static (static-function-block statement) environment return break continue throw)]
            [(and (eq? 'funcall (statement-type statement)) (list? (function-name (funcall-block statement)))) (update 
                                                                                                                    (instance-name (funcall-block statement)) 
                                                                                                                    (list (car (interpret-funcall-result-environment 
                                                                                                                                    (function-statement-list (get-funcall-closure (funcall-dot (funcall-block statement)) environment)) 
                                                                                                                                    (add-param-to-environment 
                                                                                                                                        (get-param (get-funcall-closure (funcall-dot (funcall-block statement)) environment))
                                                                                                                                        (parameters (funcall-block statement))
                                                                                                                                        (push-frame (append (lookup (funcall-instance-field (funcall-block statement)) environment) environment))
                                                                                                                                        throw
                                                                                                                                    )
                                                                                                                                    return break continue throw                                                                                                                                                                                                     
                                                                                                                                ))) environment
                                                                                                                )]
            [(eq? 'funcall (statement-type statement)) (interpret-funcall-result-environment
                                                            (function-statement-list (lookup (function-name (funcall-block statement) environment)))
                                                            (add-param-to-environment (get-param (lookup (function-name (funcall-block)) environment)) (parameters (funcall-block statement)) (push-frame environment) throw)
                                                            return break continue throw
                                                        )]
            [(eq? 'new (statement-type statement)) (interpret-new-object (new-block statement) environment return break continue throw)]
            [(eq? 'class (statement-type statement)) (interpret-class (class-block statement) environment)]
            [else (myerror "Unknown statement:" (statement-type statement))]
        )
    )
)

; abstraction for interpret-statement
(define statement-type car)
(define function-block cdr)
(define funcall-block function-block)
(define static-function-block funcall-block)
(define new-block static-function-block)
(define class-block new-block)
(define get-param car)
(define instance-name cadar)
(define funcall-dot car)
(define funcall-instance-field cadar)

(define interpret-return
    (lambda (statement environment return throw)
        (return (eval-expression (get-expresion statement) environment throw))
    )
)

(define interpret-declare
    (lambda (statement environment return break continue throw)
        (cond
            [(exists-declare-value? statement) (if (list? (get-declare-value statement))
                                                    (interpret-new-object (new-block statement) environment return break continue throw)
                                                    (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment))]
            [else (insert (get-declare-var statement) 'novalue environment)]
        )
    )
)

(define get-declare-value operand2)
(define get-declare-var operand1)

(define interpret-assign
    (lambda (statement environment)
    (let ((var-name (get-assign-lhs statement))  ; Get left-hand side of assignment
          (value (eval-expression (get-assign-rhs statement) environment)))  ; Evaluate right-hand side
      (if (string-contains? var-name ".")
          ;; Dot notation indicates an assignment to an instance field.
          (let* ((parts (split-string var-name "."))
                 (instance-name (first parts))
                 (property-name (second parts))
                 (instance (lookup instance-name environment)))  ; Find the instance in the environment
            (if instance
                (update-instance-field instance property-name value)  ; Update the field on the instance
                (error "Instance not found: " instance-name)))
          ;; Regular variable assignment.
          (let ((local-value (lookup-in-env var-name environment)))
            (if local-value
                (update environment var-name value)  ; Update existing local variable
                ;; If variable not found locally, try as non-static field of 'this'
                (let ((this-instance (lookup-in-env 'this environment)))
                  (if this-instance
                      (update-instance-field this-instance var-name value)  ; Update field on 'this'
                      (error "Variable not found in local or as a field: " var-name)))))))))

(define assign-dot-prefix cadr)
(define dot-block cadr)
(define var-to-update cddadr)
(define value-to-update caddr)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)

(define interpret-if
    (lambda (statement environment return break continue throw)
        (cond
            [(eval-expression (condition statement) environment throw) (interpret-statement (then statement) environment return break continue throw)]
            [(else? statement) (interpret-statement (get-else statement) environment return break continue throw)]
            [else environment]
        )
    )
)

(define condition operand1)
(define then operand2)
(define get-else operand3)
(define else? exists-operand3?)

(define interpret-while
    (lambda (statement environment return throw)
        (call/cc
            (lambda (break)
                (letrec
                    [(loop 
                        (lambda (condition body environment)
                            (if (eval-expression condition environment throw)
                                (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                                environment
                            )
                        )
                    )]
                    (loop (condition statement) (body statement) environment)
                )
            )
        )
    )
)

(define body operand2)

(define interpret-block
    (lambda (statement environment return break continue throw)
        (pop-frame (interpret-block-statement-list (cdr statement) (push-frame environment) return (lambda (env) (break (pop-frame env))) (lambda (env) (continue (pop-frame env))) (lambda (v env) (throw v (pop-frame env)))))
    )
)

(define interpret-block-statement-list
    (lambda (statement-list environment return break continue throw)
        (if (null? statement-list)
            environment
            (interpret-block-statement-list (remaining-lines statement-list) (interpret-statement (first-line statement-list) environment return break continue throw) return break continue throw)
        )
    )
)

(define interpret-throw
    (lambda (statement environment throw)
        (throw (eval-expression (get-expresion statement) environment throw) environment)
    )
)

(define get-expresion operand1)

(define create-throw-catch-continuation
    (lambda (catch-statement environment return break continue throw jump finally-block)
        (cond
            [(null? catch-statement) (lambda (e env) (throw e (interpret-block finally-block env return break continue throw)))]
            [(not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement")]
            [else (lambda (e env)
                    (jump (interpret-block 
                                finally-block
                                (pop-frame (interpret-throw-catch-statement-list
                                                (body catch-statement)
                                                (insert (catch-var catch-statement) e (push-frame env))
                                                return
                                                (lambda (env2) (break (pop-frame env2)))
                                                (lambda (env2) (continue (pop-frame env2)))
                                                (lambda (v env2) (throw v (pop-frame env2)))))
                            return break continue throw))
                )
            ]
        )
    )
)

(define catch-var
    (lambda (catch-statement)
        (car (operand1 catch-statement))
    )
)

(define interpret-throw-catch-statement-list
    (lambda (statement-list environment return break continue throw)
        (if (null? statement-list)
            environment
            (interpret-throw-catch-statement-list (remaining-lines statement-list) (interpret-statement (first-line statement-list) environment return break continue throw) return break continue throw)
        )
    )
)

(define interpret-try
    (lambda (statement environment return break continue throw)
        (call/cc
            (lambda (jump)
                (let*
                    [(finally-block (make-finally-block (get-finally statement)))
                    (try-block (make-try-block (get-try statement)))
                    (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
                    (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
                    (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
                    (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block))]

                    (interpret-block finally-block (interpret-block try-block environment new-return new-break new-continue new-throw) return break continue throw)
                )
            )
        )
    )
)

(define get-finally operand3)
(define get-try operand1)
(define get-catch operand2)

(define interpret-function
    (lambda (statement environment return break continue throw)
        (cond
            [(null? (function-body statement)) environment] 
            [else (insert (function-name statement) (function-body statement) environment)]
        )
    )
) 

(define function-name car)
(define function-body cdr)
(define function-statement-list cadr)

(define interpret-static
    (lambda (statement environment return break continue throw)
        (cond
            [(null? (function-body statement)) environment]
            [else (insert (function-name statement) (function-body statement) environment)]
        )
    )
)

; main class
(define evaluate-main-class
    (lambda (class-name environment return break continue throw)
        (cond
            [(not (exists? class-name environment)) (myerror "Undefined class")]
            [else (interpret-statement-list 
                        (function-statement-list (lookup-function-in-closure (cadr (lookup class-name environment)) 'main))
                        (make-layer-from-instance-fields  (cadr (lookup class-name environment)) (push-frame environment) return break continue throw)
                        class-name return break continue throw
                    )]
        )
    )
)

(define interpret-funcall-result-environment
    (lambda (statement-list environment return break continue throw)
        (cond
            [(null? statement-list) environment]
            [else (if (list? (call/cc
                                (lambda (break-return)
                                    (interpret-statement (first-line statement-list) environment break-return break continue throw))))
                        (interpret-funcall-result-environment (cdr statement-list) (call/cc
                                                                                        (lambda (break-return)
                                                                                            (interpret-statement (car statement-list) environment break-return break continue throw)
                                                                                        ))
                                                                                    return break continue throw)
                        environment
                    )
            ]
        )
    )
)

(define make-layer-from-instance-fields
    (lambda (class-closure environment return break continue throw)
        (cond
            [(null? class-closure) environment]
            [(list? (car class-closure)) (make-layer-from-instance-fields (cdr class-closure)
                                                                          (interpret-statement (car class-closure) environment return break continue throw)
                                                                          return break continue throw)]
        )
    )
)

(define interpret-funcall
    (lambda (funcall environment throw)
        (call/cc
            (lambda (return-function)
                (cond
                    [(list? (car funcall)) (interpret-function-statement-list 
                                                (funcall-closure (get-funcall-closure (funcall-dot funcall) environment)) 
                                                (add-param-to-environment (get-param (get-funcall-closure (funcall-dot funcall) environment)) (parameters funcall) (push-frame (append (lookup (instance-name-to-append funcall) environment) environment)) throw)
                                                return-function
                                                (lambda (env) (myerror "Break outside loop"))
                                                (lambda (env) (myerror "Continue used outside loop"))
                                                throw
                                            )
                    ]
                    [(null? (parameters funcall)) (interpret-function-statement-list 
                                                        (function-statement-list (lookup (function-name funcall) environment))
                                                        (push-frame (pop-frame environment)) 
                                                        return-function 
                                                        (lambda (env) (myerror "Break outside loop"))
                                                        (lambda (env) (myerror "Continue used outside loop"))
                                                        throw
                                                    )
                    ]
                    [(not (exists? (function-name funcall) environment)) (myerror "Function does not exist")]
                    [else (interpret-function-statement-list 
                                (function-statement-list (lookup (function-name funcall)))
                                (add-param-to-environment (function-name (lookup (function-name funcall) environment)) (parameters funcall) (push-frame environment) throw)
                                return-function
                                (lambda (env) (myerror "Break outside loop"))
                                (lambda (env) (myerror "Continue used outside loop"))
                                throw
                            )
                    ]
                )
            )
        )
    )
)

(define parameters cdr)
(define funcall-closure cadr)
(define instance-name-to-append cadar)

(define get-funcall-closure
  (lambda (dot-funcall environment)
    (lookup (funcall-to-look-up dot-funcall) (lookup (instance-name-to-look-up dot-funcall) environment))))

(define instance-name-to-look-up cadr)
(define funcall-to-look-up caddr)

(define add-param-to-environment
    (lambda (var val environment throw)
        (cond
            [(null? var) environment]
            [(not (eq? (length var) (length val))) (myerror "param and argument mismatch")]
            [(list? var) (add-param-to-environment (parameters var) (insert (car var) (eval-expression (car val) (pop-frame environment) throw) environment) throw)]
            [else (insert var (eval-expression val (pop-frame environment)) environment)]
        )
    )
)

(define interpret-function-statement-list
    (lambda (statement-list environment return break continue throw)
        (cond
            [(null? statement-list) (pop-frame environment)]
            [else (interpret-function-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw)]
        )
    )
)

(define interpret-dot
    (lambda (instance-name field-to-look-up environment throw)
        (cond
            [(null? instance-name) (myerror "instance name is null")]
            [(null? field-to-look-up) (myerror "field is null")]
            [(eq? instance-name 'this) (eval-expression field-to-look-up (pop-frame environment) throw)]
            [(eq? instance-name 'super) (eval-expression field-to-look-up (pop-frame (pop-frame environment)) throw)]
            [else (eval-expression field-to-look-up (append (lookup instance-name environment) environment) throw)]
        )
    )
)

(define interpret-new-object
    (lambda (statement environment return break continue throw)
        (cond
            [(null? statement) (myerror "statement does not exist")]
            [(null? (lookup (get-class-name statement) environment)) (myerror "class does not exist")]
            [else (insert (object-name statement) (make-layer-from-instance-fields (get-class-closure (lookup (get-class-name statement) environment)) (newenvironment) return break continue throw) environment)]
        )
    )
)

(define object-name car)
(define get-class-name cadadr)
(define get-class-closure cadr)

(define interpret-class
    (lambda (class-closure environment)
        (cond
            [(null? class-closure) (myerror "no closure found")]
            [else (insert (car class-closure) (cdr class-closure) environment)]
        )
    )
)

(define extend-environment-with-class
  (lambda (environment class-name)
    (cons (cons 'class-context class-name) environment)))

(define assemble-class-info
  (lambda (class-env)
    (let ((class-info (filter (lambda (entry) (or (method-definition? entry) (property-definition? entry)))
                              class-env)))
      (list->hash-table class-info))))

; Check if method is an entry
(define method-definition?
  (lambda (class-env method-name params body)
    (let ((full-params (cons 'this params)))  ; Prepend 'this' to the parameter list
      (let ((method-closure (make-closure body class-env full-params)))
        (env-define class-env method-name method-closure)))))

; Determine if an entry is a property
(define property-definition?
  (lambda (entry)
    (eq? (entry-type entry) 'property)))

(define entry-type
  (lambda (entry)
    (if (and (pair? entry) (list? (cdr entry)))
        (car (cdr entry))  ; The first item in the list is the type tag
        'unknown)))  ; Default case if no type info is found

;; Convert a list to a hash-table, assuming each element of the list is a pair (key value)
(define list->hash-table
  (lambda (lst)
    (let ((ht (make-hash)))
      (for-each (lambda (item)
                  (hash-set! ht (car item) (cdr item)))
                lst)
      ht)))


; Make-closure function to include class information
(define make-closure
  (lambda (function-body environment parameters class-info)
    (list function-body environment parameters class-info)))

(define process-dot-expression
  (lambda (lhs method-name args environment)
    ((let ((instance (evaluate-left-hand-side lhs environment)))
       (let ((class (instance-get-class instance)))
         (let ((method-closure (hash-ref (class-get-methods class) method-name)))
           (if method-closure
               (invoke-method method-closure instance args)
               (error "Method not found: " method-name))))))))

(define lookup-method-in-class
  (lambda (instance method-name)
    (let ((class-methods (instance-class-methods instance)))
      (assoc method-name class-methods))))

(define instance-class-methods
  (lambda (instance)
    (let ((class-object (instance-get-class instance)))  ; A function to get the class object from an instance
      (class-get-methods class-object))))  ; A function to get methods from the class object

;; Retrieve the methods hash table from the class object
(define (class-get-methods class)(hash-ref class 'methods))

(define instance-get-class
  (lambda (instance)
    (if (hash-has-key? instance 'class)
        (hash-ref instance 'class)
        (error "Instance does not have class information."))))

(define define-method
  (lambda (class-env method-name params body class-info)
    (let ((method-closure (make-closure body class-env params class-info)))
      (env-define class-env method-name method-closure))))

;; Retrieve the methods hash table from the class object

(define invoke-method
  (lambda (method-closure instance args)
    ((let ((method-body (car method-closure))
        (method-environment (cadr method-closure))
        (method-parameters (caddr method-closure))
        (method-class (cadddr method-closure)))  ; Class info as fourth element
    (let ((compile-time-type method-class))  ; Assuming there's a way to set or use this compile-time type
      (apply method-body (cons instance args) method-environment))))))

(define env-define
  (lambda (environment key value)
    (let ((frame (first environment)))  ; Assuming the first element is the current frame
      (if (assoc key frame)  ; Check if the key already exists in the frame
          (update environment key value)  ; Update existing binding
          (cons (cons key value) frame)))))  ; Add new binding if not present

(define evaluate-left-hand-side
  (lambda (lhs environment)
    (if (symbol? lhs)
        (lookup-env lhs environment)
        (eval-expression lhs environment))))

;; Adds field to closure
(define declare-class-field
  (lambda (class-closure field-name)
    (hash-set! class-closure 'fields (cons field-name (hash-ref class-closure 'fields '())))
    class-closure))

;; Initializes fields in instance closure
(define initialize-instance-fields
  (lambda (instance-closure class-closure)
    (let ((field-names (hash-ref class-closure 'fields '())))
      (for-each (lambda (field-name)
                  (hash-set! instance-closure field-name 'undefined))
                field-names)
      instance-closure)))

;; Checks for field
(define lookup-instance-field
  (lambda (instance-closure field-name)
    (if (hash-has-key? instance-closure field-name)
        (hash-ref instance-closure field-name)
        (error "Field does not exist: " field-name))))

;Updates the field instance
(define update-instance-field
  (lambda (instance-closure field-name new-value)
    (if (hash-has-key? instance-closure field-name)
        (hash-set! instance-closure field-name new-value)
        (error "Field does not exist: " field-name))
    instance-closure))





(define eval-expression
    (lambda (expr environment throw)
        (cond
            [(number? expr) expr]
            [(eq? expr 'true) #t]
            [(eq? expr 'false) #f]
            [(not (list? expr)) (lookup expr environment)]
            [else (eval-operator expr environment throw)]
        )
    )
)

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      ((eq? (operator expr) 'dot) (interpret-dot (dot-instance-name expr) (dot-funcall expr) environment throw))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment) environment throw)))))

(define dot-instance-name cadr)
(define dot-funcall caddr)

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? 'funcall (operator expr)) (interpret-funcall (funcall-block expr) environment throw))
      (else (myerror "Unknown operator:" (operator expr))))))



;__________________________________HELPER FUNCTIONS_______________________________________


; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

(define exists-declare-value? exists-operand2?)

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Adds a new function/closure binding pair into the environment.  Gives an error if the function already exists in this frame.
(define insert-function
  (lambda (name params body environment)
    (if (exists-in-list? name (variables (car environment)))
        (myerror "error: function is being re-declared:" name)
        (add-to-frame name (list params body) (topframe environment)))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (if (string-contains? var ".")
      (let* ((parts (split-string var "."))
               (instance-name (first parts))
               (property-name (second parts))
               (instance (eval-expression instance-name environment)))
          (lookup-instance-field instance property-name))
        ;; Regular variable lookup when there's no dot in the variable name.
        (let ((local-value (lookup-in-env var environment)))
          (if local-value
              local-value
              ;; If not found locally, attempt to look up the variable as a non-static field of 'this', if 'this' exists in the environment.
              (let ((this-instance (lookup-in-env 'this environment)))
                (if this-instance
                    (lookup-instance-field this-instance var)
                    (error "Variable not found: " var))))))))

;; Checks if there's a string
(define (string-contains? str substring) ((not (null? (regexp-match (regexp-quote substring) str)))))
;; Splits the string
(define (split-string str delimiter)(string-split str delimiter))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)  ;may not play nice with adding formal parameters
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var environment))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((nor (exists-in-list? var (variables frame))(null? frame)) (myerror (format "error: undefined variable ~a" var)))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

(define lookup-function-in-closure  
  (lambda (closure fname)
    (cond
      ((null? closure) (myerror "Function ~a not found in class closure" fname)) ;add abstraction
      ((eq? fname (cadar closure)) (cddar closure))
      (else (lookup-function-in-closure (cdr closure) fname)))))

;Lookup for environments
(define lookup-env
  (lambda (var environment)
    (cond ((null? environment) (error "Variable not found: " var))
          ((eq? (caar environment) var) (cdar environment))
          (else (lookup-env var (cdr environment))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Get the location of a name in a list of names - don't need with box implementation
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; remove first and last element of a list
(define remove-first-and-last
  (lambda (l)
    (cdr(reverse(cdr(reverse l))))))

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val)) (cdr vallist))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (new-frame environment)
    (cons new-frame environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)


; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))