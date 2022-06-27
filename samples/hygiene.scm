; (load "tests/utils.scm")

(define-syntax let
  (syntax-rules ()
    ((let ((variable init) ...) body ...)
      ((lambda (variable ...)
          body ...)
      init ...))
    ((let name ((variable init) ...) body ...)
      (letrec ((name (lambda (variable ...)
                      body ...)))
        (name init ...)))))

(define (wrap v) (list v))

(define-syntax wrap-macro
  (syntax-rules ()
    ([wrap-macro v]
    (wrap v))))

; (wrap-macro 1)

(let ((wrap (lambda (v) (+ v 100)))) (wrap-macro 1))
