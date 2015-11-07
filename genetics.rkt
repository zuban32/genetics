#lang scheme/base

(require racket/list)

(require "common.rkt")

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (fit lst cycles)
  (define (fit-vert x)
    (filter (lambda (arg) (member x arg)) cycles))
  (/ (length (remove-duplicates (foldl (lambda (arg res) (append res (fit-vert arg))) '() lst))) (length cycles)))

(define (cr-ovr x y)
  (let* ((c (gen-random (min (length x) (length y)))) (way (random 2)))
    (if (zero? way)
        (append (drop-right x c) (take-right y c))
        (append (drop-right y c) (take-right x c)))))

(define (reprod popul)
  (let ((popul (shuffle (if (odd? (length popul)) (cons (pick-random popul) popul) popul))))
    (if (null? popul) '()
        (cons (cr-ovr (car popul) (cadr popul)) (reprod (cddr popul))))))

(define (start-popul k verts popul-len)
  (define (create-popul lst len vert)
    (if (= len 0)
        lst
        (if (member vert lst)
            (create-popul lst len (pick-random verts))
            (create-popul (cons vert lst) (- len 1) (pick-random verts)))
        ))
  (if (= popul-len 0)
      '()
      (cons (create-popul '() (gen-random (+ k 1)) (pick-random verts)) (start-popul k verts (- popul-len 1)))))

(define (start-gen popul cycles)
  (let ((res (filter (lambda (arg) (= 1 (fit arg cycles))) popul)))
    (if (null? res)
        (start-gen (filter (lambda (arg) (< 0.4 (fit arg cycles))) (append popul (reprod popul))) cycles)
        res
    )
  ))

;(reprod '((2 4 6 6) (1 3 5 1 2) (1 2 3)))
;(fit '(1 3) '((1 2 3) (1 3) (1 3 4) (2 3)))
;(start-popul 5 '(1 2 3 4 5 6 7) 10)
(start-gen (remove-duplicates (start-popul 2 '(1 2 3 4 5) 10)) '((1 2 3) (4 5)))
