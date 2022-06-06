(define *tests-passed* 0)
(define *tests-failed* 0)
(define *tests-total* 0)
(define *verbose-test* #t)

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
        (define *test-id*  (if (not (eq? 'name 'expected)) (inspect name) " "))
        (incr-total)
        (if (equal? *expected* (cdr *result*))
          (begin
            (if *verbose-test* (prints "Passed..."))
            (incr-passed))
          (begin
            (incr-failed)
            (if *verbose-test* (begin
              (newline)
              (prints "!!! Failure !!!")
              (if (not (eq? 'name 'expected))
                (prints (string-pad-end " - Test Name:" 16) *test-id*))
              (prints (string-pad-end " - Expression:" 16) (inspect 'expression))
              (prints (string-pad-end " - Expected:" 16)   (inspect *expected*))
              (prints (string-pad-end " - Actual:"   16)   (inspect (cdr *result*)))))))))))

(define (test-begin name) (begin
  (prints "*** Running Tests:" name "***")
  (set-current-test name)))

(define (test-end)
  (begin
    (newline)
    (prints "--- Finished Tests:" *current-test* "---")
    (prints (string-pad-end " - Passed:" 14) *tests-passed*)
    (prints (string-pad-end " - Failed:" 14) *tests-failed*)
    (prints (string-pad-end " - Total:" 14)  *tests-total*)
    (newline)
    (set-current-test "none")
    (reset-passed)
    (reset-failed)
    (reset-total)))
