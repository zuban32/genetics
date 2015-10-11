#lang scheme/base

(provide comb)
(provide gen-random)

(define (comb lst k)
  (cond
    ((zero? k) '(()))
    ((null? lst) '())
    (else (append
           (map (lambda (x) (cons (car lst) x)) (comb (cdr lst) (sub1 k)))
           (comb (cdr lst) k)))))

; same as random, but cannot return 0
(define (gen-random num)
  (let ((res (random num)))
    (if (= 0 res) (add1 res) res)))