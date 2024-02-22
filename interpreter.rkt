#lang racket
(require "simpleParser.rkt")

(define operator (lambda (exp) (car exp)))
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define bComparator (lambda (exp) (cons (car exp) (cons (cadr exp) '()))))

(define isVar
  (lambda (x)
    (cond
      [(number? x) #f]
      [(eq? '+ x) #f]
      [(eq? '- x) #f]
      [(eq? '/ x) #f]
      [(eq? '* x) #f]
      [(eq? '% x) #f]
      [(list?  x) #f]
      [else       #t])))
  

;; Operations on 2 integers
(define M_integer
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(isVar lis)   (cdr (getBinding lis state))]
      [(eq? '/ (operator lis)) (quotient (M_integer (operand1 lis) state) (M_integer (operand2 lis) state))]
      [(eq? '* (operator lis)) (* (M_integer (operand1 lis) state) (M_integer (operand2 lis) state))]
      [(eq? '% (operator lis)) (remainder (M_integer (operand1 lis) state) (M_integer (operand2 lis) state))]
      [(eq? '+ (operator lis)) (+ (M_integer (operand1 lis) state) (M_integer (operand2 lis) state))]
      [(eq? '- (operator lis)) (- (M_integer (operand1 lis) state) (M_integer (operand2 lis) state))]
      [else 'nooperator])))



;; Operations on 1-2 booleans
(define M_boolean
  (lambda (lis state)
    (cond
      [(and (boolean? lis) (false? lis)) 'false]
      [(boolean? lis) 'true]
      [(eq? '(| |) (bComparator lis)) (or (M_boolean (operand2 lis) state) (M_boolean (operand3 lis) state))]
      [(eq? '(& &) (bComparator lis)) (and (M_boolean (operand2 lis) state) (M_boolean (operand3 lis) state))]
      [(eq? '! (operator lis)) (not (M_boolean (operand1 lis) state))]
      [else 'nooperator])))

;; Comparison ops
(define M_comparison
  (lambda (lis state)
    (cond
      [(number? lis) lis]
      [(eq? '(= =) (bComparator lis)) (eq? (M_comparison (operand2 lis) state) (M_comparison (operand3 lis) state))]
      [(eq? '(! =) (bComparator lis)) (not (eq? (M_comparison (operand2 lis) state) (M_comparison (operand3 lis) state)))]
      [(eq? '(< =) (bComparator lis)) (<= (M_comparison (operand2 lis) state) (M_comparison (operand3 lis) state))]
      [(eq? '(> =) (bComparator lis)) (>= (M_comparison (operand2 lis) state) (M_comparison (operand3 lis) state))]
      [(eq? '< (operator lis)) (< (M_comparison (operand2 lis) state) (M_comparison (operand3 lis) state))]
      [(eq? '> (operator lis)) (> (M_comparison (operand2 lis) state) (M_comparison (operand3 lis) state))]
      [else 'nooperator])))



;; TODO statements - currently tentative

;; Declare operation
(define M_declare
  (lambda (lis state)
    (cond
      [(null? lis) '()]
      [(eq? 'var (operator lis)) (addBinding (operand1 lis) state)]
      [else (error "Unsupported operation" lis)])))

;; Assign operation
(define M_assign
  (lambda (lis state)
    (cond
      [(null? lis) '()]
      [(eq? '= (operator lis)) (setBinding (cons (operand1 lis) (cons (M_value (operand2 lis) state) '())) state)]
      [else (error "Unsupported operation" lis)])))

;; If operation
(define M_if
  (lambda (lis state)
    (if (M_boolean (operand1 lis state) state)
        (M_state (operand2 lis) state)
        (M_state (operand3 lis) state))))

;; While operation
(define M_while
  (lambda (lis state)
    (if (M_boolean (operand1 lis state) state)
        (M_while lis (M_state (operand2 lis) state))
        state)))

(define getBinding ;; takes a variable name and a state
  (lambda (x state)
    (cond
      [(null? state)             (error 'noSuchBinding x)] 
      [(eq? x (car (car state))) (car state)]
      [else                      (getBinding x (cdr state))])))

(define setBinding ;; takes a binding pair and a state
  (lambda (x state)
    (cond
      [(null? state)                                      '()]
      [(and (list? (car state)) (eq? (getBinding (car x) state) 'noSuchBinding))    (error 'noSuchVariable (car x))]
      [(list? (car state))                                (cons (setBinding x (car state)) (setBinding x (cdr state)))]
      [(eq? (car x) (car state))                          (cons (car state) (cons (cadr x) '())) ]
      [else                                               (cons (car state) (setBinding x (cdr state)))])))

(define addBinding ;; takes a variable name and a state
  (lambda (x state)
    (cond
      [(null? state) (cons x '())]
      [(not(null?(getBinding x state))) (error 'redefinedVariable x)]
      [else (cons x state)])))
        
    
;; store the state in the stack, use tail recursion to continuously read and alter the state as you recursively go through the program

; state

(define M_state
  (lambda (exp state)
    (cond
      [(null? exp) (car state)]
      [(eq? (operator exp) 'var) (M_declare exp state)]
      [(eq? (operator exp) '=) (M_assign exp state)]
      [(eq? (operator exp) 'if) (M_if exp state)]
      [(eq? (operator exp) 'while) (M_while exp state)]
      [(eq? (operator exp) 'return) (M_value exp state)]
      [else (error "Unsupported operation" exp)])))

;; Value operation
;; Value operation
(define M_value
  (lambda (exp state)
    (cond
      [(null? exp) (error "Empty expression")]
      [(number? exp) exp]
      [(or (eq? exp 'true)  (eq? exp 'false)) exp]
      [(boolean? (operand1 exp)) (M_boolean (operand1 exp) state)]
      [(number? (operand1 exp)) (M_integer exp state)]
      [(list? (operand1 exp)) (M_value (cons (operator exp) (cons (M_value (operand1 exp) state) (cons (operand2 exp) '()))) state)]
      [else (error "Invalid expression" exp)])))


(define program (lambda (file) (parser file)))


(define interpret
  (lambda (filename)
    (if (null? (parser filename))
        (error "Empty program")
        (call/cc (lambda (k) (evaluate (parser filename) k '()))))))

(define evaluate
  (lambda (parse break state)
    (cond
      [(eq? (operator (car parse)) 'var)
       (M_declare (car parse) state)
       (evaluate (cdr parse) break state)]
      [(eq? (operator (car parse)) 'return)
       (break (M_value (cadr(car parse)) state))]
      [(eq? (operator (car parse)) 'if)
       (M_if (car parse) state)
       (evaluate (cdr parse) break state)]
      [(eq? (operator (car parse)) 'while)
       (M_while (car parse) state)
       (evaluate (cdr parse) break state)]
      [(eq? (operator (car parse)) '=)
       (M_assign (car parse) state)
       (evaluate (cdr parse) break state)]
      [else (error "Unsupported operation or invalid syntax")])))

