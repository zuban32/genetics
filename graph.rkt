#lang scheme/base
(require racket/list)
(require "common.rkt")

(provide gen-graphs)
(provide find-cycles)
(provide count-verts)
(provide get-verts)

(define (gen-graph num)
  (define (gen-edge)
    (let ((in (random num))(out (random num)))
      (if (= in out) (cons in (+ (gen-random 5) out)) (cons in out))))
  (define (add-edge cur)
    (if (< cur 0) '()
        (cons (gen-edge) (add-edge (- cur 1)))))
  (remove-duplicates (add-edge (+ (/ num 2) (random num)))))

(define (gen-graphs iter bound)
  (if (< iter bound)
      (cons (gen-graph (gen-random 30)) (gen-graphs (add1 iter) bound))
      '()))

(define (count-verts graph)
  (foldl (lambda (a res)(max (car a) (cdr a) res)) 0 graph))

(define (get-verts graph)
  (remove-duplicates (foldl (lambda(arg result)(cons (car arg)(cons (cdr arg) result))) '() graph)))

(define (cyclic? lst1 lst2)
  (let* ((tail (member (car lst1) lst2))(pos (- (length lst2) (if (equal? #f tail) 0 (length tail)))))
    (equal?
     lst1
     (append (drop lst2 pos) (take lst2 pos))
     )))

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


(define (find-cycles graph)
  (dfs graph))