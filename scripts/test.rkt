#lang racket/base
; #lang r5rs

; (require racket/pretty)
(require macro-debugger/stepper)

; (define-syntax macro-or
;   (syntax-rules ()
;     ([macro-or] #f)
;     ([macro-or x xs ...]
;      (let ((v x))
;       (if v v (macro-or xs ...))))))

; (expand/step #'(let ((v (list 1 2 3))) (display "first") (display "done")))

(define-syntax let (syntax-rules ()
  ((let ((variable init) ...) body ...)
    ((lambda (variable ...)
        body ...)
     init ...))
  ((let name ((variable init) ...) body ...)
    (letrec ((name (lambda (variable ...)
                     body ...)))
      (name init ...)))))

(define-syntax letrec (syntax-rules ()
  ((letrec ((variable init) ...) body ...)
    ((lambda ()
      (define variable init) ...
      body ...)))))

(define-syntax fluid-let
  (syntax-rules ()
    ((_ () expr . exprs)
      (begin expr . exprs))
    ((_ ((v1 a1) (v2 a2) ...) expr . exprs)
      (let ((outer-v v1))
        (set! v1 a1)
        (fluid-let ((v2 a2) ...)
          (let ((r (begin expr . exprs)))
            (set! v1 outer-v)
            r))))))

(expand/step #'(fluid-let ((a 1)) (f)))

; (expand/step #'(let ((=> #f)) (cond (#t => 'ok))))
; (expand/step #'`(,@foo))

; (display `((`foo' ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
