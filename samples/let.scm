; macro definition
; (load 'stdlib/r5rs)

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))))

(let ([x (+ 2 2)]) (writeln (+ x x)))

; (let ((r (- x (* y q)))) (if (eqv? r 0) 0 (if (eqv? (< x 0) (< y 0)) r (+ r y))))

; (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))
;  (let* ((z (+ x y))) (* z x))
; (let ((x 2) (y 3)) (* x y))

; (tlist
;   ((tlist
;     ((regular-id lambda.1)
;      (tlist ((ellipsis-template (pattern-id name) 1)))
;      (pattern-id body1)
;      (ellipsis-template (pattern-id body2) 1)))
;    (ellipsis-template (pattern-id val) 1)))

; (let* ((x 7) (z (+ x y))) (* z x))

; (let ((a 1) (b 2))
;   (display "adding two numbers")
;   (display (+ a b)))

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

; (define-syntax let
;   (syntax-rules ()
;     ((let ((name val) ...) body1 body2 ...)
;       ((lambda (name ...) body1 body2 ...)
;       val ...))))

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


; (let ((a 1) (b 2))
;   (display "adding two numbers\\n")
;   (+ a b))

; EXPECTED
;; ((lambda.1 (a b) (display "adding two numbers\n") (+ a b)) 1 2)

;;ACTUAL
;; ((lambda.1 (a b) ((display "adding two numbers\\n")) (+ a b)) 1 2)


; (define id (x) x)

; (define-syntax broke
;   (syntax-rules ()
;     ((broke ((name1 val1) ...) body1 body2 ...)
;       (print ((name1 val1) ...) body1 body2 ...))))

; (broke ((a 1) (b 2)) (* z z))

; (let ((x 2) (y 3))
;     (let* ((x 7)
;           (z (+ x y)))
;       (* z x)))

; (let* ((x 7) (z (+ x y))) (* z x))

; (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))


; (and (= 2 2) (> 2 1))

; (define-syntax and
;   (syntax-rules ()
;     ([and] #t)
;     ([and test] test)
;     ([and test1 test2 ...]
;       (if test1 [and test2 ...] #f))))

; (and (> 2 1))
; (and)

; (if (> 3 2) 'yes 'no)