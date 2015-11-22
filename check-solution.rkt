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
  (genetics-solve graph max #f))
;(start-bruteforce graph max))

(define (run-test tests acc succ total notmin)
  (if (null? tests)
      (begin
        (printf "Tests passed: ~v of ~v\n" succ (sub1 total))
        (printf "Notmin: ~v ~v\n" (length notmin) notmin)
        (if (equal? #t acc) (printf "Success!\n") (printf "Fail!\n"))
        )
      (let* (
             (result
              (flatten (solve (caaar tests) (cdaar tests)))
              )
             (eq (or
                  (and (equal? #f (car result)) (equal? #f (caadar tests)))
                  (and (equal? (car result) (caadar tests)) (equal? #t (answer-correct? result (caaar tests)))))))
        (begin
          (printf "Running test #~v\n" total)
          ;(printf "Result: ~v\n" result)
          ;(printf "Correct result: ~v\n" (cadar tests))
          (if (equal? eq #t)
              (begin 
                (printf "Passed\n")
                (if (and (not (equal? #f (car result))) (not (equal? (cadr result)(cadr (cadar tests)))))
                    (begin
                      (printf "Not minimal\n\n")
                      (run-test (cdr tests) (and acc eq) (add1 succ) (add1 total) (cons total notmin)))
                    (begin
                      (printf "\n")
                      (run-test (cdr tests) (and acc eq) (add1 succ) (add1 total) notmin)
                      )))
              (begin
                (printf "Failed\n\n")
                (run-test (cdr tests) (and acc eq) succ (add1 total) notmin)))
          ))
      ))

(define (check-solution)
  (run-test (read-tests) #t 0 1 '()))

(check-solution)