#lang racket/base

(require racket/pretty)
(require macro-debugger/stepper)

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
      ((lambda (name ...) body1 body2 ...)
      val ...))))

(expand/step #'(let ((a 1) (b 2))
  (display "adding two numbers\n")
  (+ a b)))