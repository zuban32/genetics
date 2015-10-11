#lang scheme/base
(require racket/list)
(require "solve.rkt")

(define (read-single-test in)
  (let* ((test (list (read in) (read in))))
    (begin
      ;(printf "Test\n")
      ;(writeln test)
      (if (eof-object? (car test))
          '()
          (append (read-single-test in) (list test))))))

(define (read-tests)
  (call-with-input-file "tests.txt" (lambda(in)(read-single-test in))))

(define (solve graph max)
  (start-bruteforce graph max))

(define (run-test tests acc succ total max)
  (if (null? tests)
      (begin
        (printf "Tests passed: ~v of ~v\n" succ (sub1 total))
        (if (equal? #t acc) (printf "Success!\n") (printf "Fail!\n"))
        )
      (let* ((result (flatten (solve (caar tests) max)))(eq (equal? result (cadar tests))))
        (begin
          ;(printf "Current test ~v\n" (car tests))
          (printf "Running test #~v\n" total)
          (printf "Result: ~v\n" result)
          (printf "Correct result: ~v\n" (cadar tests))
          (if (equal? eq #t)
              (begin 
                (writeln 'Passed)
                (run-test (cdr tests) (and acc eq) (add1 succ) (add1 total) max))
              (begin
                (writeln 'Failed)
                (run-test (cdr tests) (and acc eq) succ (add1 total) max)))
          ))
      ))

(define (check-solution max)
  (run-test (read-tests) #t 0 1 max))

(check-solution (read))