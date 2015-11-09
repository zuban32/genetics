#lang scheme/base

(require racket/list)

(require "common.rkt")
(require "graph.rkt")
(require "solve.rkt")

(provide genetics-solve)

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

(define MAX_ITER 30)

(define (genetics-solve graph k)
  (define (start-gen popul cycles iter)
    
    ; absolutely useless function because of memory amout it needs 
    (define (check-cycles)
      (begin (printf "checking cycles\n")
             (let ((combs (comb cycles (+ k 1))))
               (ormap (lambda(x) (
                                  (let ((part-combs (comb combs 2)))
                                    (andmap (lambda(arg)(not(intersects? (car arg) (cdr arg)))) part-combs))))
                      combs))))
    
    (define (fit1 lst)
      (define (fit-vert x)
        (filter (lambda (arg) (member x arg)) cycles))
      (/ (length (remove-duplicates (foldl (lambda (arg res) (append res (fit-vert arg))) '() lst))) (length cycles)))
    
    (define (fit2 lst)
      (- 1 (/ (length (filter (lambda (vert) (ormap (lambda(cycle) (member vert cycle)) cycles)) lst))
              (length lst))))
    
    (define (fit lst)
      (* (fit1 lst) 100))
    
    (define (top lst num)
      (define (top-step cur len)
        (if (= 0 len)
            '()
            (cons (car cur) (top-step (cdr cur) (- len 1)))))
      (top-step (sort lst > #:key fit) (floor num)))
    
    
    (define (mutate popul verts)
      (define (mutate-person lst)
        (if (member lst (top popul (/ (length popul) 4)))
            (remove-duplicates (map (lambda (arg) (if (= 0 (random 10)) (pick-random verts) arg)) lst))
            lst))
      (define (mutate-step lst num)
        (if (< num 1)
            lst
            (cons (mutate-person (car lst)) (mutate-step (cdr lst) (- num 1)))
            ))
      (mutate-step (shuffle popul) (length popul))
      )
    
    (define (cr-ovr x y)
      (let* ((c (gen-random (min (length x) (length y)))))
        (list (append (drop-right x c) (take-right y c))
              (append (drop-right y c) (take-right x c)))
        )
      )
    
    (define (reprod popul)
      (let ((popul (shuffle (if (odd? (length popul)) (cons (pick-random popul) popul) popul))))
        (if (null? popul)
            '()
            (append (cr-ovr (car popul) (cadr popul)) (reprod (cddr popul))))))
    
    (begin ;(println iter)
           ;(println (length popul))
           (if (or
                (= 0 iter)
                (null? cycles)
                (= 0 (length popul))
                )
               (list (list #f))
               (let ((res (filter (lambda (arg) (= 100 (fit arg))) popul)))
                 (if (null? res)
                     (let* (
                            (mean (average (map fit popul)))
                            ;(surv (filter (lambda (arg) (< mean (fit arg))) popul))
                            (new-popul (append popul (reprod popul)))
                            (surv (top new-popul (length popul)))
                            )
                       (begin
                         ;(printf "Mean = ~v\n" mean)
                         (start-gen (mutate surv (get-verts graph)) cycles (- iter 1))))
                     res
                     ))
               )
           ))
  (let* ((cycles (find-cycles graph)) (res (argmin length (start-gen (map (lambda (x) (normalize x cycles)) (start-popul k (get-verts graph) 50)) cycles MAX_ITER))))
    (if (equal? (car res) #f)
        #f
        (list (not (equal? (car res) #f)) (length res) res))))

(define simple-graph
  '((1 . 2) (2 . 3) (3 . 1) (4 . 5) (5 . 4)))


;(let* ((test-graph (cons (gen-complete-graph 6) 6))(res (genetics-solve (car test-graph) (cdr test-graph))))
;(let* ((test-graph (gen-graph 25)) (res (genetics-solve (car test-graph) (cdr test-graph))))
; (begin
;(println (find-cycles (car test-graph)))
; (printf "k = ~v\n" (cdr test-graph))
;  (println res)
;(println (start-bruteforce (car test-graph) (cdr test-graph)))))

;(append (list '(1 2 3) '(1 2 5)) (list '(1 3 5) ' (1 5 4)) ' ())