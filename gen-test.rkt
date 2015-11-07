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
   (list
    (cons (gen-cyclic-graph GRAPH_CYCLE_LENGTH) GRAPH_CYCLE_LENGTH)
    (cons (gen-acyclic-graph GRAPH_CYCLE_LENGTH) GRAPH_CYCLE_LENGTH)
    )
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
                                       ))
                                 ))
                graphs)
      (let ((complete-graph (gen-complete-graph MAX_COMPLETE_GRAPH)))
        (begin 
          (writeln (cons complete-graph MAX_COMPLETE_GRAPH) file)
          (writeln (gen-complete-graph-answer complete-graph MAX_COMPLETE_GRAPH) file)
          (writeln (cons complete-graph (- MAX_COMPLETE_GRAPH 3)) file)
          (writeln (gen-complete-graph-answer complete-graph (- MAX_COMPLETE_GRAPH 3)) file)
          ))
      (close-output-port file)
      )))

(gen-test)
