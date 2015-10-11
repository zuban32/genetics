#lang scheme/base
(require racket/list)
(require "graph.rkt")
(require "solve.rkt")

(define (gen-test K)
  
  (let ((file (open-output-file "tests.txt" #:mode 'text #:exists 'replace)) (graphs (gen-graphs 0 3)))
    (begin 
      (for-each (lambda (graph)(begin
                                 (writeln graph file)
                                 (let ((result (start-bruteforce graph K)))
                                   (if (equal? result #f)
                                       (writeln (list result) file)
                                       (begin
                                         (writeln (flatten result) file)
                                         ;(writeln (car result) file)
                                         ;(writeln (cadr result) file)
                                         ;(writeln (caddr result) file)
                                         ))
                                   )))
                graphs)  
      (close-output-port file)
      )
    ))

(gen-test (read))

;(list (cons 0 1) (cons 1 2) (cons 2 0) (cons 3 4) (cons 4 5) (cons 5 3))