#lang scheme/base
(require racket/list)

(define (gen-test K)
  
  (define (gen-length num)
    (let ((res (random num)))
      (if (= 0 res) (gen-length num) res)))
  
  (define (gen-graph num)
    (define (gen-edge)
      (let ((in (random num))(out (random num)))
        (if (= in out) (gen-edge) (cons in out))))
    (define (add-edge cur)
      (if (< cur 0) '()
          (cons (gen-edge) (add-edge (- cur 1)))))
    (remove-duplicates (add-edge (+ (/ num 2) (random num)))))
  
  (define (count-verts graph)
    (foldl (lambda (a res)(max (car a) (cdr a) res)) 0 graph))
  
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
    (begin
      (printf "Graph: ~v\n" graph)
      
      (remove-duplicates (foldl
                          (lambda (vert result)
                            (begin
                              (append (foldl
                                       (lambda (edge result1) (append (start-dfs edge) result1))
                                       '()
                                       (filter (lambda (edge) (= (car edge) vert)) graph)) result)
                              
                              ))
                          '()
                          (range (add1 (count-verts graph)))) cyclic?)
      ))
  
  
  (define (comb lst k)
    (cond
      ((zero? k) '(()))
      ((null? lst) '())
      (else (append
             (map (lambda (x) (cons (car lst) x)) (comb (cdr lst) (sub1 k)))
             (comb (cdr lst) k)))))
  
  (define (bruteforce graph cycles depth)
    (if (< K depth)
        #f
        (let ((result (ormap(lambda(verts)
                              (andmap(lambda(cycle)
                                       (ormap (lambda(cycle-vert)(member cycle-vert verts))
                                              cycle))
                                     cycles))
                            (comb (range (add1 (count-verts graph))) depth))))
          (if (equal? result
                      #f
                      )
              (bruteforce graph cycles (add1 depth))
              result))))
  
  (define (find-cycles graph)
    (dfs graph));(gen-graph(read))))
  
  (define (start-bruteforce graph)
    (bruteforce graph (find-cycles graph) 1))
  (start-bruteforce  (list (cons 0 1) (cons 1 2) (cons 2 0) (cons 3 4) (cons 4 5) (cons 5 3)))
  )

(gen-test 2)