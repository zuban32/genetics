#lang scheme/base
(require racket/list)
(require "graph.rkt")
(require "solve.rkt")

(define GRAPH_CYCLE_LENGTH 200)
(define RANDOM_GRAPHS_NUM 100)
(define MAX_COMPLETE_GRAPH 6)

;generates graphs for testing
(define (gen-test-graphs)
  (append
   (let ((complete-graph (gen-complete-graph MAX_COMPLETE_GRAPH)))
     (list
      (cons (gen-cyclic-graph GRAPH_CYCLE_LENGTH) 3)
      (cons (gen-acyclic-graph GRAPH_CYCLE_LENGTH) 3)
      (cons complete-graph (sub1 MAX_COMPLETE_GRAPH))
      (cons complete-graph (- MAX_COMPLETE_GRAPH 2))
      ))
   (gen-graphs RANDOM_GRAPHS_NUM)
   ))

(define (gen-test)
  (let ((file (open-output-file "tests.txt" #:mode 'text #:exists 'replace)) (graphs (gen-test-graphs)))
    (begin 
      (for-each (lambda (graph)(begin
                                 (writeln graph file)
                                 (let ((result (start-bruteforce (car graph) (cdr graph))))
                                   (if (equal? result #f)
                                       (writeln (list result) file)
                                       (writeln (flatten result) file)
                                       ))))
                graphs)  
      (close-output-port file)
      )
    ))

(gen-test)

;(list (cons 0 1) (cons 1 2) (cons 2 0) (cons 3 4) (cons 4 5) (cons 5 3))