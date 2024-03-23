#lang racket
(require "simpleParser.rkt")

; defines quick operators
(define operator (lambda (exp) (car exp)))
(define operand1 (lambda (exp) (cadr exp)))
(define operand2 caddr)
(define operand3 cadddr)
(define depth1 car)
(define depth2 caar)
(define depth2b caadr)
(define depth3 caaar)

; defines what a comparison operator is
(define isCompOp
  (lambda (x)
    (cond
      [(eq? '!= x) #t]
      [(eq? '== x) #t]
      [(eq? '<= x) #t]
      [(eq? '>= x) #t]
      [(eq? '> x) #t]
      [(eq? '< x) #t]
      [else       #f])))

; defines what a variable is
(define isVar
  (lambda (x)
    (cond
      [(number? x) #f]
      [(eq? '+ x) #f]
      [(eq? '- x) #f]
      [(eq? '/ x) #f]
      [(eq? '* x) #f]
      [(eq? '% x) #f]
      [(eq? '= x) #f]
      [(eq? 'true x) #f]
      [(eq? 'false x) #f]
      [(list?  x) #f]
      [else       #t])))

; defines what an integer operator is is
(define isIntOp
  (lambda (x)
    (cond
      [(eq? '+ x) #t]
      [(eq? '- x) #t]
      [(eq? '/ x) #t]
      [(eq? '* x) #t]
      [(eq? '% x) #t]
      [else       #f])))

; invert a boolean value
(define invert
  (lambda (x)
    (if (eq? x 'true)
        'false
        'true)))

; check if target indicates a new block
(define isBlock
  (lambda (x)
    (if (eq? x 'begin )
        #t
        #f)))

; convert to #t and #f from true and false
(define convertBool
  (lambda (x)
    (if (eq? x 'true)
        #t
        #f)))

; returns the length of a list
(define mylength
  (lambda (lis)
    (if (null? lis)
        0
        (+ 1 (mylength (cdr lis))))))
  
;; Operations on 2 integers
(define M_integer
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(isVar lis)   (cdr (getBinding lis state))]
      [(and (eq? '- (operator lis)) (eq? 2 (mylength  lis))) (- (M_value (operand1 lis) state))]
      [(eq? '/ (operator lis)) (quotient (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [(eq? '* (operator lis)) (* (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [(eq? '% (operator lis)) (remainder (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [(eq? '+ (operator lis)) (+ (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [(eq? '- (operator lis)) (- (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [else 'nooperator])))

;; Operations on 1-2 booleans
(define M_boolean
  (lambda (lis state)
    (cond
      [(and (boolean? lis) (false? lis))'false]
      [(boolean? lis) 'true]
      [(eq? '|| (operator lis)) (M_boolean(or (convertBool (M_value (operand1 lis) state)) (convertBool(M_value (operand2 lis) state))) state)]
      [(eq? '&& (operator lis)) (and (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [(eq? '! (operator lis)) (invert (M_value (operand1 lis) state))]
      [(isCompOp (operator lis)) (M_value (M_comparison lis state) state)]
      [else 'nooperator])))

;; Comparison ops
(define M_comparison
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(eq? '== (operator lis)) (eq? (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [(eq? '!= (operator lis)) (not (eq? (M_value (operand1 lis) state) (M_value (operand2 lis) state)))]
      [(eq? '<=(operator lis)) (<= (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [(eq? '>= (operator lis)) (>= (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [(eq? '< (operator lis)) (< (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [(eq? '> (operator lis)) (> (M_value (operand1 lis) state) (M_value (operand2 lis) state))]
      [else 'nooperator])))

;; Declare operation
(define M_declare   ;if there is a sublist in the list containing var set the value of the declared variable
  (lambda (lis state)
    (cond
      [(null? lis) '()]
      [(eq? 'var (operator lis)) (addBinding (operand1 lis) state)]
      [else (error "Unsupported operation" (symbol->string (operator lis)))])))

;; Assign operation
(define M_assign
  (lambda (lis state)
    (cond
      [(null? lis) '()]
      [(and (eq? '= (operator lis))(eq? (M_value (operand2 lis) state) 'noSuchBinding)) (error 'noSuchBinding(symbol->string (operand2 lis)))] ;this
      [(and (isVar (operator lis)) (eq? (getBinding (operator lis) state) 'noSuchBinding))    (error 'noSuchBinding)]
      [(and (eq? '= (operator lis)) (eq? (getBinding (operand1 lis) state) 'noSuchBinding))    (error 'noSuchBinding)]
      [(eq? '= (operator lis)) (setBinding (cons (operand1 lis) (cons (M_value (operand2 lis) state) '())) state)]
      [(isVar (operator lis))  (setBinding (cons (operator lis) (cons (M_value (operand1 lis) state) '())) state)]
      [else (error "Unsupported operation" (symbol->string (operator lis)))])))

;; If operation
(define M_if
  (lambda (lis return state)
    (if (> (mylength lis) 3)
        (if (eq? 'true (M_boolean (operand1 lis) state))
            (M_state (operand2 lis) return state)
            (M_state (operand3 lis) return state))
        (if (eq? 'true (M_boolean (operand1 lis) state))
            (M_state (operand2 lis) return state)
            state))))

;; While operation
; (define M_while
;   (lambda (lis return state)
;     (if (eq? 'true (M_boolean (operand1 lis) state))
;         (M_while lis return (M_state (operand2 lis) return state))
;         state)))

(define M_while
  (lambda (lis return state break continue throw next)
    (if (eq? 'true (M_boolean (operand1 lis) state))
        (M_state (operand2 lis) return state
            (lambda (v1) (next v1)) ; break
            (lambda (v2) (M_while lis return v2 break continue throw next)) throw ; continue
            (lambda (v3) (M_while lis return v3 break continue throw next)) ; next
        )
        (next state)
    )
  )
)

;; try catch finally
(define M_try_catch
  (lambda (lis return state break continue throw next)
      (M_state (operand1 lis) 
          ; return
          (lambda (v s) (M_state (finallyblock exp) return s break continue throw (lambda (s1) (return v s1)))) state
          ; break
          (lambda (s) (M_state (finallyblock exp) return s break continue throw break))
          ; continue
          (lambda (s) (M_state (finallyblock exp) return s break continue throw continue))
          ; throw
          (lambda (e s) (M_state (finallyblock exp) 
              ; return
              (lambda (v1 s1) (M_state (finallyblock exp) return s1 break continue throw (lambda (s2) (return v1 s2))))
              ; state
              (setBinding (cons (operand1 (operand2 exp)) (cons e '())) s)
              ; break
              (lambda (s) (M_state (finallyblock exp) return s break continue throw break))
              ; continue
              (lambda (s) (M_state (finallyblock exp) return s break continue throw continue))
              ; throw
              (lambda (e1 s1) (M_state (finallyblock exp) return s1 break continue throw (lambda (s2) (throw e1 s2))))
              ; next
              (lambda (s) (M_state (finallyblock exp) return s break continue throw next))
          ))
          ; next
          (lambda (s) (M_state (finallyblock exp) return s break continue throw next))
      )
  )
)

;; abstraction for finally
(define finallyblock
  (lambda (exp)
    (if (null? (operand3 exp))
        '()
        (operand1 (operand3 exp))
    )
  )
)

(define getBinding ;; takes a variable name and a state
  (lambda (x state)
    (cond
      [(null? state)             'noSuchBinding] 
      [(eq? x (depth2 state))    (depth1 state)]
      [else                      (getBinding x (cdr state))])))

(define setBinding ;; takes a binding pair and a state
  (lambda (x state)
    (cond
      [(null? state)                                      '()]
      [(eq? (depth1 x) (depth2 state))                    (cons (cons (depth2 state) (cons (cadr x) '())) '()) ]
      [(list? (depth2 state))                             (cons (setBinding x (depth2 state)) (setBinding x (operand1 state)))]
      [else                                               (cons (depth2 state) (setBinding x (operand1 state)))])))

(define addBinding ;; takes a variable name and a state
  (lambda (x state)
    (cond
      [(null? state) (cons(cons x '())'())]
      [(eq? (depth1 state) '()) (cons(cons x '()) (depth1 state))]
      [(not(eq?(getBinding x state) 'noSuchBinding)) (error 'redefinedVariable (symbol->string x))] 
      [else (cons (cons x '()) state)])))

;; store the state in the stack, use tail recursion to continuously read and alter the state as you recursively go through the program

; dedicate a block function to execute code within it, add a layer and remove a layer on exit
; M_block first then remove layer and continue eval

; block function

;needs to return state

(define M_block
  (lambda (exp return state)
    (if (null? (operand1 exp))
      (addLayer state)
      (evaluate (cdr exp) return (addLayer state)))))

(define addLayer
  (lambda (state)
    (display (list '() state))
    (list '() state)))

(define removeLayer
  (lambda (state)
    (cdr state)))

; state function
(define M_state
  (lambda (exp return state break continue throw next)
    (cond
      [(null? exp) (car state)]
      [(and (> (mylength exp) 2) (eq? (operator exp) 'var))
       (M_assign (cdr exp) (M_declare exp state))]
      [(eq? (operator exp) 'var) (M_declare exp state)]
      [(eq? (operator exp) '=) (M_assign exp state)]
      [(eq? (operator exp) 'if) (M_if exp return state)]
      [(eq? (operator exp) 'while) (call/cc (lambda (v) (M_while exp return state v continue throw next)))]
      [(eq? (operator exp) 'return) (return (M_value exp state))]
      [(eq? (operator exp) 'break) (break state)]
      [(eq? (operator exp) 'continue) (continue state)]
      [(eq? (operator exp) 'try) (M_try_catch exp return (addLayer state) )]
      [else (error "Unsupported operation" (symbol->string exp))])))

;; Value operation
(define M_value
  (lambda (exp state)
    (cond
      [(null? exp) (error "Empty expression")]
      [(list? exp)
           (cond
             [(isIntOp (operator exp)) (M_integer exp state)]
             [(eq? 'return (operator exp))  (M_value (operand1 exp) state)]
             [else (M_boolean exp state)])]
      [(number? exp) exp]
      [(boolean? exp) (M_boolean exp state)]
      [(and (isVar exp)(eq? 'noSuchBinding (getBinding exp state))) (error 'noSuchBinding (symbol->string exp))]
      [(and (isVar exp)(eq? (mylength (getBinding exp state)) 1)) (display (getBinding exp state))(error 'bindingUnassigned (symbol->string exp))]
      [(isVar exp)(operand1 (getBinding exp state))]
      [(or (eq? exp 'true)  (eq? exp 'false)) exp]
      [(boolean? (operand1 exp)) (M_boolean (operand1 exp) state)]
      [(number? (operand1 exp)) (M_integer exp state)]
      [(list? (operand1 exp)) (M_value (cons (operator exp) (cons (M_value (operand1 exp) state) (cons (operand2 exp) '()))) state)]
      [else (error "Invalid expression"
                   '("Expression" value)' exp)])))

; defines a file as a program
(define program (lambda (file) (parser file)))

; inteprets a file
(define interpret
  (lambda (filename)
    (begin
      (display "Full Parse")
      (display (parser filename))
      (newline))
    (if (null? (parser filename))
        (error "Empty program")
        (call/cc (lambda (v) (evaluate (parser filename) v '()))))))

; mblock should return a state
(define evaluate
  (lambda (parse return state break continue throw next)
    (cond
      [(isBlock (operator (car parse)))
       (evaluate (cdr parse) return (removeLayer (M_block (car parse) return state)))]
      [(eq? (operator (car parse)) 'var)
       (evaluate (cdr parse) return (M_state (car parse) return state))]
      [(eq? (operator (car parse)) 'return)
       (return (M_value (operand1(car parse)) state))]
      [(eq? (operator (car parse)) 'if)
       (evaluate (cdr parse) return (M_if (car parse) return state))]
      [(eq? (operator (car parse)) 'while)
       (evaluate (cdr parse) return (call/cc (lambda (v) (M_while (car parse) return state v continue throw next))))] ;; (call/cc (lambda (v) (M_while exp return state v continue throw next)))
      [(eq? (operator (car parse)) '=)
       (evaluate (cdr parse) return (M_state (car parse) return state))]
      [else (error "Unsupported operation or invalid syntax")])))

