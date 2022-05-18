; macro definition
; (load "./fac.scm")

; (define-syntax let
;   (syntax-rules ()
;     ((let ((name val) ...) body1 body2 ...)
;      ((lambda (name ...) body1 body2 ...)
;       val ...))))

; (tlist
;   ((tlist
;     ((regular-id lambda.1)
;      (tlist ((ellipsis-template (pattern-id name) 1)))
;      (pattern-id body1)
;      (ellipsis-template (pattern-id body2) 1)))
;    (ellipsis-template (pattern-id val) 1)))

; (define-syntax let*
;   (syntax-rules ()
;     ((let* () body1 body2 ...)
;       (let () body1 body2 ...))
;     ((let* ((name1 val1) (name2 val2) ...)
;         body1 body2 ...)
;       (let ((name1 val1))
;         (let* ((name2 val2) ...)
;           body1 body2 ...)))))

; (let* ((x 7) (z (+ x y))) (* z x))

(let ((a 1) (b 2))
  (display "adding two numbers")
  (display (+ a b)))

; macro invocations

; (let ((a 5))
;   (display "squaring a number")
;   (display (* a a)))

; (let ((a 1) (b 2))
;   (display "adding two numbers")
;   (display (+ a b)))

; (let ((x 2) (y 3))
;   (let* ((x 7)
;          (z (+ x y)))
;         (* z x)))

; (display
;   (let ((x 2) (y 3))
;     (display "doing funky stuff")
;     (let* ((x 7)
;           (z (+ x y)))
;           (* z x))))

; (tlist
;   ((regular-id let.1)
;    (tlist ((tlist ((pattern-id name1) (pattern-id val1)))))
;    (tlist
;     ((regular-id let*.1)
;      (tlist
;       ((ellipsis-template (tlist ((pattern-id name2) (pattern-id val2))) 1)))
;      (pattern-id body1)
;      (ellipsis-template (pattern-id body2) 1)))))

; (print
;   (let ((x 2) (y 3))
;     (let* ((x 7)
;           (z (+ x y)))
;       (* z x)))
; ) ; => 70
