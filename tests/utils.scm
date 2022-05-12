(define *tests-passed* 0)
(define *tests-failed* 0)
(define *tests-total* 0)
(define *verbose-test* #t)

(define *current-test* "none")

(defun incr-passed () (set! *tests-passed* (+ 1 *tests-passed*)))
(defun incr-failed () (set! *tests-failed* (+ 1 *tests-failed*)))
(defun incr-total  () (set! *tests-total*  (+ 1 *tests-total*)))
(defun reset-passed () (set! *tests-passed* 0))
(defun reset-failed () (set! *tests-failed* 0))
(defun reset-total  () (set! *tests-total*  0))

(defun set-current-test (name) (set! *current-test* name))
(defun set-verbose-test (to) (set! *verbose-test* to))

(define-macro test-id (id expected expression)
  `(begin
      (define *name*     ,id)
      (define *result*   (cadr (try (lambda () ,expression))))
      (define *expected* ,expected)
      (incr-total)
      (if (equal? *expected* *result*)
        (begin
          (if *verbose-test* (print "Passed..."))
          (incr-passed))
        (begin
          (incr-failed)
          (if *verbose-test* (begin
            (newline)
            (print "!!! Failure !!!")
            (print " - Name:      " ',expression)
            (print " - Expression:" ',expression)
            (print (string-pad-end " - Expected:" 16) *expected*)
            (print (string-pad-end " - Actual:"   16) *result*)))))))

(define-macro test (expected expression)
  `(begin
      (define *result*   (try (lambda () ,expression)))
      (define *expected* ,expected)
      (incr-total)
      (if (equal? *expected* (cadr *result*))
        (begin
          (if *verbose-test* (print "Passed..."))
          (incr-passed))
        (begin
          (incr-failed)
          (if *verbose-test* (begin
            (newline)
            (print "!!! Failure !!!")
            (print " - Expression:" ',expression)
            (print (string-pad-end " - Expected:" 16) *expected*)
            (print (string-pad-end " - Actual:"   16) (cadr *result*))))))))

(defun test-begin (name) (begin
  (print "*** Running Tests:" name "***")
  (set-current-test name)))

(defun test-end ()
  (begin
    (newline)
    (print "--- Finished Tests:" *current-test* "---")
    (printn (string-pad-end " - Passed:" 14) *tests-passed*)
    (printn (string-pad-end " - Failed:" 14) *tests-failed*)
    (printn (string-pad-end " - Total:" 14) *tests-total*)
    (newline)
    (set-current-test "none")
    (reset-passed)
    (reset-failed)
    (reset-total)))
