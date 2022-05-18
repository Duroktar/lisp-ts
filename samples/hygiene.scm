; (load "tests/utils.scm")

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))))

(defun wrap (v) (list v))

(define-syntax wrap-macro
  (syntax-rules ()
    ([wrap-macro v]
     (wrap v))))

; (wrap-macro 1)

; (test 1 (let ((wrap (lambda (v) (+ v 100)))) (wrap (wrap-macro 1))))
(let ((wrap (lambda (v) (+ v 100)))) (wrap (wrap-macro 1)))
