#lang racket
(require "simpleParser.rkt")

(define operator (lambda (exp) (car exp)))
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define bComparator (cons car (cons cadr '())))

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











