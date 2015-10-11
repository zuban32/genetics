#lang scheme/base
(require racket/list)

(define (gen-test K)
  
  (define (gen-length num)
    (let ((res (random num)))
      (if (= 0 res) (gen-length num) res)))
  
  (define (gen-graph num)
    (define (add-edge cur)
      (if (> 0 cur) '()
          (cons (cons (random num)(random num)) (add-edge (- cur 1)))))
    (remove-duplicates (add-edge (+ (/ num 2) (random num)))))
  
  (define (count-verts graph)
    (foldl (lambda (a res)(max (car a) (cdr a) res)) graph))
  
  (define (start-dfs graph cycles vis end)
    
    (define (dfs edge graph cur-cycle cycles)
      (begin (printf "Vert ~v\n" (car edge))
      (if (and (not (member (car edge) end)) (member (car edge) vis))
          (cons (cons (car edge) cur-cycle) cycles)
          (let ((vis (cons (car edge) vis)))
           (for-each
            (lambda (arg)(dfs arg graph (cons (car edge) cur-cycle) cycles))
            (filter (lambda (arg) (= (cadr edge)(car arg))) graph)
            )         
           (cons (car edge) end)
           ))))
    
    (for-each (lambda (arg) (dfs arg graph '() cycles)) graph)
    cycles)
  
  
  (define (find-cycles)
    (start-dfs (list (list 0 1) (list 1 2) (list 2 3) (list 3 0)) '() '() '()))
  
  (find-cycles)
  )

(gen-test 0)