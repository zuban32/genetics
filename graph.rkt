#lang scheme/base
(require racket/list)
(require "common.rkt")

(provide gen-graphs)
(provide find-cycles)
(provide count-verts)
(provide get-verts)
(provide gen-edge)
(provide gen-complete-graph)
(provide gen-cyclic-graph)
(provide gen-acyclic-graph)

(define MAX_EDGES 50)

(define (get-verts graph)
  (remove-duplicates (foldl (lambda(arg result)(cons (car arg)(cons (cdr arg) result))) '() graph)))

(define (count-verts graph)
  (length (get-verts graph)))

; creates an edge from IN to OUT
(define (gen-edge in out)
  (if (= in out) (cons in (+ (gen-random 5) out)) (cons in out)))

; graph edges count depends on NUM
(define (gen-graph num)
  (define (add-edge cur)
    (if (< cur 0) '()
        (cons (gen-edge (random num) (random num)) (add-edge (sub1 cur)))))
  (let ((result (add-edge (+ num (random num)))))
    (cons (remove-duplicates result) (gen-random (count-verts result)))))

; generates complete graph with NUM vertices
(define (gen-complete-graph num)
  (define (add-edge iter)
    (if (< iter num)
        (append (foldl (lambda (arg result) (if (= arg iter) result (cons (cons iter arg) result))) '() (range num)) (add-edge (add1 iter)))
        '()))
  (add-edge 0))

(define (gen-cyclic-graph max)
  (define (add-edge cur)
    (cond ((= cur (sub1 max)) (list (gen-edge (sub1 max) 0)))
          (else (cons (gen-edge cur (add1 cur)) (add-edge (add1 cur))))
          ))
  (remove-duplicates (cons (gen-edge (floor (/ max 2)) 0) (add-edge 0))))

(define (gen-acyclic-graph max)
  (define (add-edge cur)
    (cond ((= cur (sub1 max)) (list (gen-edge 0 (sub1 max))))
          (else (cons (gen-edge cur (add1 cur)) (add-edge (add1 cur))))
          ))
  (remove-duplicates (add-edge 0)))

; generates MAX random graphs
(define (gen-graphs max)
  (define (iter i)
    (if (< i max)
        (cons (gen-graph (gen-random MAX_EDGES)) (iter (add1 i)))
        '()))
  (iter 0))

; checks whether LST2 is a cyclic permutation of LST1
(define (cyclic? lst1 lst2)
  (let* ((tail (member (car lst1) lst2))(pos (- (length lst2) (if (equal? #f tail) 0 (length tail)))))
    (equal?
     lst1
     (append (drop lst2 pos) (take lst2 pos))
     )))

; starts depth-first search for GRAPH
(define (dfs graph)
  (define (dfs-step edge vis cur-cycle)
    (begin
      (if (member (cdr edge) vis)
          (list (cons (car edge) (member (cdr edge)(reverse cur-cycle))))
          (foldl
           (lambda (arg result) (append (dfs-step arg (cons (car edge) vis) (cons (car edge) cur-cycle)) result))
           '()
           (filter (lambda (arg) (= (cdr edge) (car arg))) graph)
           )
          )))
  
  (define (start-dfs edge)
    (begin
      (let ((cycle (dfs-step edge '() '())))
        cycle 
        )))
  
  (remove-duplicates (foldl
                      (lambda (vert result)
                        (begin
                          (append (foldl
                                   (lambda (edge result1) (append (start-dfs edge) result1))
                                   '()
                                   (filter (lambda (edge) (= (car edge) vert)) graph)) result)
                          
                          ))
                      '()
                      (range (add1 (count-verts graph)))) cyclic?))

; returns all cycles existing in GRAPH 
(define (find-cycles graph)
  (dfs graph))