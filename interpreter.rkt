#lang racket
(require "simpleParser.rkt")

(define operator (lambda (exp) (car exp)))
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define bComparator (lambda (exp) (cons (car exp) (cons (cadr exp) '()))))

;; Operations on 2 integers
(define M_integer
  (lambda (lis)
    (cond
      [(number? lis) lis]
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

;; TODO statements

(define M_assign
  (lambda (lis)
    (cond
      [(null? lis) '()]
      [(eq? '= (operator lis)) (cons operand1 operand2)]
      [else 'nooperator])))

(define M_declare
  (lambda (lis state)
    (cond
      [(null? lis) '()]
      [(eq? 'var (operator lis)) (cons operand1 state)]
      [else 'nooperator])))

(define getBinding
  (lambda (x state)
    (cond
      [(null? state) 'noSuchBinding]
      [(eq? x (car (car state))) (car state)]
      [else                       (getBinding x (cdr state))])))

(define setBinding
  (lambda (x state)
    (cond
      [(null? state) 'noSuchBinding]
      [(eq? x (cadr (car state))) (car state)]
      [else                       (getBinding x (cdr state))])))
        
    



;; store the state in the stack, use tail recursion to continuously read and alter the state as you recursively go through the program









