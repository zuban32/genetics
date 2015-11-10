#lang scheme/base
(require racket/list)
(require "solve.rkt")
(require "graph.rkt")
(require "common.rkt")
(require "genetics.rkt")

(define (answer-correct? answer graph)
  (let ((cycles (find-cycles graph)))
    (if (andmap(lambda(cycle)
                 (if (intersects? cycle answer) answer #f))
               cycles)
        #t
        #f)))

(define (read-single-test in)
  (let* ((test (list (read in) (read in))))
    (begin
      (if (eof-object? (car test))
          '()
          (append (read-single-test in) (list test))))))

(define (read-tests)
  (call-with-input-file "tests.txt" (lambda(in)(read-single-test in))))

(define (solve graph max)
  (genetics-solve graph max))
  ;(start-bruteforce graph max))

(define (run-test tests acc succ total failed)
  (if (null? tests)
      (begin
        (printf "Tests passed: ~v of ~v\n" succ (sub1 total))
        (printf "Failed: ~v\n" failed)
        (if (equal? #t acc) (printf "Success!\n") (printf "Fail!\n"))
        )
      (let* ((result (flatten (solve (caaar tests) (cdaar tests))))(eq (or (and (equal? #f (car result)) (equal? #f (caadar tests))) (and (equal? (car result) (caadar tests)) (equal? #t (answer-correct? result (caaar tests)))))))
        (begin
          (printf "Running test #~v\n" total)
          ;(printf "Result: ~v\n" result)
          ;(printf "Correct result: ~v\n" (cadar tests))
          (if (equal? eq #t)
              (begin 
                (printf "Passed\n")
                (printf "\n")
                (run-test (cdr tests) (and acc eq) (add1 succ) (add1 total) failed))
              (begin
                (printf "Failed\n\n")
                (run-test (cdr tests) (and acc eq) succ (add1 total) (cons total failed))))
          ))
      ))

(define (check-solution)
  (run-test (read-tests) #t 0 1 '()))

(check-solution)