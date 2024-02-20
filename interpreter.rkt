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
      [(eq? '+ (operator lis)) (+ (M_integer(operand1 lis)) (M_integer(operand2 lis)))]
      [(eq? '- (operator lis)) (- (M_integer(operand1 lis)) (M_integer(operand2 lis)))]
      [(eq? '/ (operator lis)) (quotient (M_integer(operand1 lis)) (M_integer(operand2 lis)))]
      [(eq? '* (operator lis)) (* (M_integer(operand1 lis)) (M_integer(operand2 lis)))]
      [(eq? '% (operator lis)) (remainder (M_integer(operand1 lis)) (M_integer(operand2 lis)))]
      [else 'nooperator])))


;; Operations on 1-2 booleans
(define M_boolean
  (lambda (lis)
    (cond
      [(boolean? lis) lis]
      [(eq? '(| |) (bComparator lis)) (or (M_boolean (operand2 lis)) (M_boolean (operand3 lis)))]
      [(eq? '(& &) (bComparator lis)) (and (M_boolean (operand2 lis)) (M_boolean (operand3 lis)))]
      [(eq? '!     (operator lis))    (not(M_boolean (operand1 lis))) ]
      [else 'nooperator])))

;(define M_value
;  (lambda (lis)))

;; Comparison ops

(define M_comparison
  (lambda (lis)
    (cond
      [(number? lis) lis]
      [(eq? '(= =) (bComparator lis)) (eq? (M_comparison(operand2 lis)) (M_comparison(operand3 lis))) ]
      [(eq? '(! =) (bComparator lis)) (not(eq? (M_comparison(operand2 lis)) (M_comparison(operand3 lis)))) ]
      [(eq? '(< =) (bComparator lis)) (<= (M_comparison(operand2 lis)) (M_comparison(operand3 lis))) ]
      [(eq? '(> =) (bComparator lis)) (>= (M_comparison(operand2 lis)) (M_comparison(operand3 lis))) ]
      [(eq? '<     (operator lis))    (< (M_comparison(operand2 lis)) (M_comparison(operand3 lis)))]
      [(eq? '>     (operator lis))    (> (M_comparison(operand2 lis)) (M_comparison(operand3 lis)))]
      [else 'nooperator])))

;; TODO statements - currently tentative i think M_value is needed to do these properly

(define M_assign
  (lambda (lis state)
    (if (eq? '= (operator lis))
       (setBinding (cons operand1 (cons operand2 '())) state)
       'nooperator)))

(define M_declare
  (lambda (lis state)
    (if (eq? 'var (operator lis))
      (cons (cons operand1 '()) state)
      'nooperator)))

(define M_if
  (lambda (lis state)
    (cond
      [(operand1) operand2] 
      [(eq? operand3 'else) (cdr operand3)]
      [else 'nooperator])))

(define M_while
  (lambda (lis state)
    (cond
      [(operand1) operand2] ;;need to evaluate repeatedly not sure how
      [else 'nooperator])))






(define getBinding ;; takes a variable name and a state
  (lambda (x state)
    (cond
      [(null? state) (error 'noSuchBinding x)] ;; change to error later
      [(eq? x (car (car state))) (car state)]
      [else                       (getBinding x (cdr state))])))

(define setBinding ;; takes a binding pair and a state
  (lambda (x state)
    (cond
      [(null? state)                                      '()]
      [(and (list? (car state)) (eq? (getBinding (car x) state) 'noSuchBinding))    (error 'noSuchVariable (car x))]
      [(list? (car state))                                (cons (setBinding x (car state)) (setBinding x (cdr state)))]
      [(eq? (car x) (car state))                          (cons (car state) (cons (cadr x) '())) ]
      [else                                               (cons (car state) (setBinding x (cdr state)))])))
        
    
;; store the state in the stack, use tail recursion to continuously read and alter the state as you recursively go through the program

; state

(define M_state
  (lambda (exp state)
    (cond
      [(null? exp) (car state)]
      [(eq? (operator exp) 'var)
       (M_state (cdr exp) (cons (cons (cadr exp) 0) state))]
      [(eq? (operator exp) 'return)
       (cons (M_integer (operand1 exp) state) state)]
      [else (error "Unsupported operation" exp)])))



(define program (lambda (file) (parser file)))

(define interpret
  (lambda (filename)
    (cond
      [(eq? (operator (car program)) 'var)]
      [else 'lol])))




