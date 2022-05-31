#lang racket/base

(require racket/pretty)

(pretty-print 
  '(begin (define *result* (try (lambda () (begin (define x 28) x)))) (define *expected* 28) (define *test-id* (if (not (eq? ''#4 '28)) (inspect '#4) " ")) (incr-total) (if (equal? *expected* (cadr *result*)) (begin (if *verbose-test* (prints "Passed...")) (incr-passed)) (begin (incr-failed) (if *verbose-test* (begin (newline) (prints "!!! Failure !!!") (if (not (eq? ''#4 '28)) (prints (string-pad-end " - Test Name:" 16) *test-id*)) (prints (string-pad-end " - Expression:" 16) (inspect '(begin (define x 28) x))) (prints (string-pad-end " - Expected:" 16) (inspect *expected*)) (prints (string-pad-end " - Actual:" 16) (inspect (cadr *result*))))))))
  
)
