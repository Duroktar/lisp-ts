(define *tests-passed* 0)
(define *tests-failed* 0)
(define *tests-total* 0)
(define *verbose-test* #f)

(define *current-test* "none")

(define (incr-passed)  (set! *tests-passed* (+ 1 *tests-passed*)))
(define (incr-failed)  (set! *tests-failed* (+ 1 *tests-failed*)))
(define (incr-total)   (set! *tests-total*  (+ 1 *tests-total*)))
(define (reset-passed) (set! *tests-passed* 0))
(define (reset-failed) (set! *tests-failed* 0))
(define (reset-total)  (set! *tests-total*  0))

(define (set-current-test name) (set! *current-test* name))
(define (set-verbose-test to) (set! *verbose-test* to))

(define-syntax test
  (syntax-rules ()
    ([test expect expr]
     (test expect expect expr))
    ([test name expected expression]
      (begin
        (define *result*   (try (lambda () expression)))
        (define *expected* expected)
        (define *test-id*  (if (not (eq? 'name 'expected)) (describe name) " "))
        (incr-total)
        ; (if *verbose-test* (displayln (string-append "Testing .. " (describe *expected*))))
        ; (if *verbose-test* (displayln (string-append "Result .. " (describe (cdr *result*)))))
        (if (equal? *expected* (cdr *result*))
          (begin
            (if *verbose-test* (displayln "Passed..."))
            (incr-passed))
          (begin
            (incr-failed)
            (if *verbose-test* (begin
              (newline)
              (displayln "!!! Failure !!!")
              (if (not (eq? 'name 'expected))
                (displayln (string-append (string-pad-end " - Test Name: " 16) *test-id*)))
              (displayln (string-append (string-pad-end " - Expression: " 16) (describe 'expression)))
              (displayln (string-append (string-pad-end " - Expected: " 16)   (describe *expected*)))
              (displayln (string-append (string-pad-end " - Actual: "   16)   (describe (cdr *result*))))))))))))

(define (test-begin name) (begin
  (displayln (string-append "*** Running Tests: " name " ***"))
  (set-current-test name)))

(define (test-end)
  (begin
    (newline)
    (displayln (string-append "--- Finished Tests: " *current-test* " ---"))
    (displayln (string-append (string-pad-end " - Passed: " 14) *tests-passed*))
    (displayln (string-append (string-pad-end " - Failed: " 14) *tests-failed*))
    (displayln (string-append (string-pad-end " - Total: " 14)  *tests-total*))
    (newline)
    (set-current-test "none")
    (reset-passed)
    (reset-failed)
    (reset-total)))
