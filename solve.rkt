#lang scheme/base

(require racket/list)
(require "graph.rkt")
(require "common.rkt")

(provide start-bruteforce)

(define (start-bruteforce graph max)
  (bruteforce graph (find-cycles graph) 1 max))

(define (bruteforce graph cycles depth max)
  (if (or (< max depth) (null? cycles))
      #f
      (let ((result (ormap(lambda(verts)
                            (andmap(lambda(cycle)
                                     (ormap (lambda(cycle-vert)(member cycle-vert verts))
                                            cycle))
                                   cycles))
                          (comb (get-verts graph) depth))))
        (if (equal? result
                    #f
                    )
            (bruteforce graph cycles (add1 depth) max)
            (list #t (length result) result)))))
