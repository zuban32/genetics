#lang scheme/base

(provide comb)
(provide gen-random)
(provide pick-random)
(provide intersects?)
(provide full-in?)
(provide average)


(define (prob n1 n2)
  (if (= 0 n1)
      0
      (< (random n1) n2)))

(define (comb lst k)
  (cond
    ((zero? k) '(()))
    ((null? lst) '())
    (else (append
           (map (lambda (x) (cons (car lst) x)) (comb (cdr lst) (sub1 k)))
           (comb (cdr lst) k)))))

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (intersects? lst1 lst2)
  (foldl (lambda (arg result) (or result (not (equal? #f (member arg lst2))))) #f lst1))

(define (full-in? lst1 lst2)
  (and (<= (length lst1) (length lst2)) (if (andmap (lambda (arg) (member arg lst2)) lst1) #t #f)))

; same as random, but cannot return 0
(define (gen-random num)
  (let ((res (random num)))
    (if (= 0 res) (add1 res) res)))

(define (average lst)
  (if (null? lst)
      0
      (/ (foldl + 0 lst)
         (length lst))))