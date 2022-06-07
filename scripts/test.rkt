#lang racket/base

(require racket/pretty)
(require macro-debugger/stepper)

; (define-syntax macro-or
;   (syntax-rules ()
;     ([macro-or] #f)
;     ([macro-or x xs ...]
;      (let ((v x))
;       (if v v (macro-or xs ...))))))

(expand/step #'(let ((v (list 1 2 3))) (display "first") (display "done")))
