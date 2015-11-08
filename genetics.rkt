#lang scheme/base

(require racket/list)

(require "common.rkt")
(require "graph.rkt")
(require "solve.rkt")

(provide genetics-solve)

(define (mutate popul verts)
  (define (mutate-person lst)
    (remove-duplicates (map (lambda (arg) (if (= 0 (random 3)) (pick-random verts) arg)) lst)))
  (define (mutate-step lst num)
    (if (< num 1)
        lst
        (cons (mutate-person (car lst)) (mutate-step (cdr lst) (- num 1)))
        ))
  (mutate-step (shuffle popul) (/ (length popul) 3))
  )

;(mutate '((1 2 3) (2 4 6) (3 5 7 8 9) (1 3 5)) (range 10))

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

(define (gen-out-verts verts cycles)
  (define (norm-step lst res int-lst)
    (define (check-vert arg)
      (foldl (lambda (arg1 result) (or result (and (not (equal? (car arg) (car arg1)))(not (member (car arg1) res)) (full-in? (cdr arg) (cdr arg1))))) #f int-lst))
    (if (null? lst)
        res
        (norm-step (cdr lst)
                   (if (check-vert (car lst))
                       (cons (caar lst) res)
                       res)
                   int-lst)
        
        ))
  (let ((int-lst (map (lambda (arg) (cons arg (filter (lambda (arg1) (member arg arg1)) cycles))) verts)))
    (norm-step int-lst '() int-lst)))

(define (normalize verts cycles)
  (let ((out (gen-out-verts verts cycles)))
    (filter (lambda(arg)(not (member arg out))) verts)))

(define MAX_ITER 25)

(define (genetics-solve graph k)
  (define (start-gen popul cycles iter)   
    (define (fit1 lst)
      (define (fit-vert x)
        (filter (lambda (arg) (member x arg)) cycles))
      (/ (length (remove-duplicates (foldl (lambda (arg res) (append res (fit-vert arg))) '() lst))) (length cycles)))
    
    
    (define (fit2 lst)
      (- 1 (/ (length (filter (lambda (vert) (ormap (lambda(cycle) (member vert cycle)) cycles)) lst))
              (length lst))))
    
    (define (fit lst)
      (fit1 lst))
    
    ;(fit1 '(1 4) '((1 2 3) (4 5)))
    ;(fit '(1 4) '((1 2 3) (4 5)))
    
    (define (cr-ovr x y)
      (let* ((c (gen-random (min (length x) (length y)))) (way (random 2))) ;(mom (argmin length (list x y))) (dad (argmax length (list x y))))
        (if (zero? way)
            (append (drop-right x c) (take-right y c))
            (append (drop-right y c) (take-right x c)))))
    
    (define (reprod popul)
      (let ((popul (shuffle (if (odd? (length popul)) (cons (pick-random popul) popul) popul))))
        (if (null? popul) '()
            (cons (normalize (cr-ovr (car popul) (cadr popul)) cycles) (reprod (cddr popul))))))
    
    ;(begin (println iter)
     ;      (println (length popul))
           (if (or (= iter 0) (null? cycles))
               (list (list #f))
               (let ((res (filter (lambda (arg) (= 1 (fit arg))) popul)) (mean (* (average (map fit popul)) 1)))
                 (if (null? res)
                     (start-gen (filter (lambda (arg) (< mean (fit arg))) (mutate (append popul (reprod popul)) (get-verts graph))) cycles (- iter 1))
                     res
                     ))
      ;         )
           ))
  (let* ((cycles (find-cycles graph)) (res (argmin length (start-gen (map (lambda (x) (normalize x cycles)) (start-popul k (get-verts graph) 100)) cycles MAX_ITER))))
    (if (equal? (car res) #f)
        #f
        (list (not (equal? (car res) #f)) (length res) res))))

(define simple-graph
  '((1 . 2) (2 . 3) (3 . 1) (4 . 5) (5 . 4)))

;(reprod '((2 4 6 6) (1 3 5 1 2) (1 2 3)))
;(fit '(1 3) '((1 2 3) (1 3) (1 3 4) (2 3)))
;(start-popul 5 '(1 2 3 4 5 6 7) 10)
;(let* ((test-graph (cons (gen-complete-graph 6) 6))(res (genetics-solve (car test-graph) (cdr test-graph) 25)))
(let* ((test-graph (gen-graph 25)) (res (genetics-solve (car test-graph) (cdr test-graph))))
  (begin
    ;(println (find-cycles (car test-graph)))
    (println res)
    ;(println (normalize (cadr res) (find-cycles (car test-graph))))
    (println (start-bruteforce (car test-graph) (cdr test-graph)))))
