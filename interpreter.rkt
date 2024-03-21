#lang racket
(require "simpleParser.rkt")

; defines quick operators
(define operator (lambda (exp) (car exp)))
(define operand1 (lambda (exp) (cadr exp)))
(define operand2 caddr)
(define operand3 cadddr)
;(define bComparator (lambda (exp) (cons (car exp) (cons (cadr exp) '()))))

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

; convert #t and #f to true and false
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

;; TODO statements - currently tentative

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
      [(and (eq? '= (operator lis))(eq? (M_value (operand2 lis) state) 'noSuchBinding)) (error 'noSuchBinding(symbol->string (operand2 lis)))]
      [(and (isVar (operator lis)) (eq? (getBinding (operator lis) state) 'noSuchBinding))    (error 'noSuchBinding)]
      [(and (eq? '= (operator lis)) (eq? (getBinding (operand1 lis) state) 'noSuchBinding))    (error 'noSuchBinding)]
      [(eq? '= (operator lis)) (setBinding (cons (operand1 lis) (cons (M_value (operand2 lis) state) '())) state)]
      [(isVar (operator lis))  (setBinding (cons (operator lis) (cons (M_value (operand1 lis) state) '())) state)]
      [else (error "Unsupported operation" (symbol->string (operator lis)))])))

;; If operation
(define M_if
  (lambda (lis break state)
    (if (> (mylength lis) 3)
        (if (eq? 'true (M_boolean (operand1 lis) state))
            (M_state (operand2 lis) break state)
            (M_state (operand3 lis) break state))
        (if (eq? 'true (M_boolean (operand1 lis) state))
            (M_state (operand2 lis) break state)
            state))))
            
;(evaluate (cdr parse) break (M_state (car parse) state))]

;; While operation
(define M_while
  (lambda (lis break state next breakExp)
    (if (eq? 'true (M_boolean (operand1 lis) state))
        (M_while lis break (M_state (operand2 lis) break state))
        (next state))))

(define loop
  (lambda (lis break state next breakExp)
    (if (eq? 'true (M_boolean (operand1 lis) state))
        (M_while lis break )
    )
  )
)

(define repeat
  (lambda (lis break state next)
    
  )
)

(define getBinding ;; takes a variable name and a state
  (lambda (x state)
    (cond
      [(null? state)             'noSuchBinding] 
      [(eq? x (car (car state))) (car state)]
      [else                      (getBinding x (cdr state))])))

(define setBinding ;; takes a binding pair and a state
  (lambda (x state)
    (cond
      [(null? state)                                      '()]
      [(eq? (car x) (car state))                          (cons (car state) (cons (cadr x) '())) ]
      [(list? (car state))                                (cons (setBinding x (car state)) (setBinding x (cdr state)))]
      [else                                               (cons (car state) (setBinding x (cdr state)))])))

(define addBinding ;; takes a variable name and a state
  (lambda (x state)
    (cond
      [(null? state) (cons(cons x '()) '())]
      [(not(eq?(getBinding x state) 'noSuchBinding)) (error 'redefinedVariable (symbol->string x))] 
      [else (cons (cons x '()) state)])))
    
;; store the state in the stack, use tail recursion to continuously read and alter the state as you recursively go through the program

; state function
(define M_state
  (lambda (exp break state)
    (cond
      [(null? exp) (car state)]
      [(and (> (mylength exp) 2) (eq? (operator exp) 'var))
       (M_assign (cdr exp) (M_declare exp state))]
      [(eq? (operator exp) 'var) (M_declare exp state)]
      [(eq? (operator exp) '=) (M_assign exp state)]
      [(eq? (operator exp) 'if) (M_if exp break state)]
      [(eq? (operator exp) 'while) (M_while exp break state)]
      [(eq? (operator exp) 'return) (break(M_value exp state))]
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
      [(and (isVar exp)(eq? (mylength (getBinding exp state)) 1)) (error 'bindingUnassigned (symbol->string exp))]
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
    (if (null? (parser filename))
        (error "Empty program")
        (call/cc (lambda (k) (evaluate (parser filename) k '()))))))

(define evaluate
  (lambda (parse break state)
    (cond
      [(eq? (operator (car parse)) 'var)
       (evaluate (cdr parse) break (M_state (car parse) break state))]
      [(eq? (operator (car parse)) 'return)
       (break (M_value (operand1(car parse)) state))]
      [(eq? (operator (car parse)) 'if)
       (evaluate (cdr parse) break (M_if (car parse) break state))]
      [(eq? (operator (car parse)) 'while)
       (evaluate (cdr parse) break (M_while (car parse) break state))]
      [(eq? (operator (car parse)) '=)
       (evaluate (cdr parse) break (M_state (car parse) break state))]
      [else (error "Unsupported operation or invalid syntax")])))

