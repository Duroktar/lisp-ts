import { env } from "./globals";
import { Lisp } from "./lib/lisp";

Lisp.execute(`
(define *tests-total* 0)
(define *tests-passed* 0)
(define *tests-failed* 0)

(defun incr-passed () (set! *tests-passed* (+ 1 *tests-passed*)))
(defun incr-failed () (set! *tests-failed* (+ 1 *tests-failed*)))
(defun incr-total  () (set! *tests-total*  (+ 1 *tests-total*)))

(define-macro test (id expected body)
  \`(begin
      (incr-total)
      (if (= ,expected ,body)
        (begin (incr-passed) (printn ,id "Match"))
        (begin (incr-failed) (printn ,id "No Match")))
    )
)

(defun test-begin ()
  (print "Running Tests")
  (newline))

(defun test-end ()
  (begin
    (newline)
    (print "Testing Complete")
    (printn "Passed:" *tests-passed*)
    (printn "Failed:" *tests-failed*)
    (printn "Total:" *tests-total*)))


(test-begin)

(test '#1 5 (+ 3 2))
(test '#2 6 (+ 3 3))

(test-end)

`, env);
