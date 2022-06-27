
; ((let loop ((x 1))
;   (cond ((> x 10)   (print "We're done!"))
;         (else       (loop (+ x 1))))))

; (cond
;     ((> 1 10)   "We're done!")
;     (else       (+ 1 1)))


; (let ((loop (lambda () (cond
;     ((> counter 0) (begin
;         (display counter)
;         (set! counter (- counter 1))
;         (loop))
;     )))))
;   (display (loop)))

; (cond
;     ((> counter 0) (begin
;         (display counter)
;         (set! counter (- counter 1))))
;     (#t (display "Done!"))
; )

; ((lambda () (cond
;     ((> counter 0) (begin
;         (display counter)
;         (set! counter (- counter 1))
;         (display 'Doner)))
; )))

; (
;     (lambda (loop) (loop))
;     ((lambda () (cond
;         ((> counter 0) (begin
;             (display counter)
;             (set! counter (- counter 1))
;             (display 'Doner)))
;     )) '())
; )

; (let ((loop (lambda () (cond
;     ((> counter 0) (begin
;         (display counter)
;         (set! counter (- counter 1))
;         (display 'Doner)))
; )))) (loop))

; (let ((i 55) (k 55)) (display (+ i k)))

; ((λ (loop) (loop) ((λ () (cond (((> counter 0) (begin ((display counter) (set! counter (- counter 1)) (display 'Doner))))))))))

; ((λ (i k) (display (+ i k)) (55 55)))

; (let merp () (display (+ 6 6)))


; ; before:
; (let my-loop ((x 1))
;   (if (> x 10)
;     (print "We're done!")
;     (my-loop (+ x 1))))

; ; after:
; ((lambda ()
;     (define my-loop (lambda (x)
;                         (if (> x 10)
;                             (print "We're done!")
;                             (my-loop (+ x 1)))))
;     (my-loop 1)))

; ((lambda () (begin
;     (define my-loop (lambda (x)
;                         (if (> x 10)
;                             (print "We're done!")
;                             (my-loop (+ x 1)))))
;     (my-loop 1))))


; (let my-loop ((x 1))
;   (cond ((> x 10) (print "We're done!"))
;     (else (my-loop (+ x 1)))))

; (display (macroexpand '(define add4
;   (let ((x 4))
;     (lambda (y) (+ x y))))))

; (define add4
;   (let ((x 4))
;     (lambda (y) (+ x y))))

; (define add4 ((lambda (x) (lambda (y) (+ x y))) 4))


; ((lambda ()
;     (begin
;         (define loop (lambda ()
;             (cond (((
;                 (> counter 0) ((begin (display counter)) (loop))
;             ) ())))))
;         (loop))))



; (cond
;     ((> 1 10)   "We're done!")
;     (else       (+ 1 1)))

; (let fac ([n 10])
;     (if (zero? n)
;         1
;         (* n (fac (sub1 n)))))

; (let ([x 5])
;   (let ([x 6])
;     x))

; (define (fac n)
;     (if (zero? n)
;         1
;         (* n (fac (sub1 n)))))

; (define-syntax cond
;     (syntax-rules (else =>)
;       ([cond (else result1 result2 ...)]
;        (begin result1 result2 ...))
;       ([cond (test => result)]
;        (let ((temp test))
;          (if temp (result temp))))
;       ([cond (test => result) clause1 clause2 ...]
;        (let ((temp test))
;          (if temp
;              (result temp)
;              (cond clause1 clause2 ...))))
;       ([cond (test)] test)
;       ([cond (test) clause1 clause2 ...]
;        (let ((temp test))
;          (if temp
;              temp
;              (cond clause1 clause2 ...))))
;       ([cond (test result1 result2 ...)]
;        (if test (begin result1 result2 ...)))
;       ([cond (test result1 result2 ...)
;              clause1 clause2 ...]
;        (if test
;            (begin result1 result2 ...)
;            (cond clause1 clause2 ...)))))

; (print (cond (#t 'yep)))


; ;; expected

; (tlist
;   ((tlist
;     ((regular-id lambda)
;      (tlist ((ellipsis-template (pattern-id name) 1)))
;      (pattern-id body1)
;      (ellipsis-template (pattern-id body2) 1)))
;    (ellipsis-template (pattern-id val) 1)))

; ;; actual

; (tlist
;   ((tlist
;     ((regular-id lambda)
;      (tlist ((ellipsis-template (pattern-id name) 1)))
;      (pattern-id body1)
;      (ellipsis-template (pattern-id body2) 1)))
;    (ellipsis-template (pattern-id val) 1)))


; ((lambda.1 (a b) (display "adding two numbers\n") (+ a b)) 1 2)
; ((lambda (a b) ((display "adding two numbers")) (+ a b)) 1 2)

; (let ((x 2) (y 3)) (* x y))

; (define-syntax and
;   (syntax-rules ()
;     ([and] #t)
;     ([and test] test)
;     ([and test1 test2 ...]
;       (if test1 [and test2 ...] #f))))


; ;; ----------------------------------------------------------------
; (define-syntax show
;   (syntax-rules ()
;     [(show expr ...)
;      (begin
;        (begin (write 'expr) (display "=") (write expr) (newline))
;        ...)]))

; (show (+ 1 2) (/ 3 4))
; ;; ----------------------------------------------------------------


; ;; ----------------------------------------------------------------
; (define (wrap v) (list v))

; (define-syntax wrap-macro
;   (syntax-rules ()
;     ([wrap-macro v]
;     (wrap v))))

; (wrap-macro 1)

; (let ((wrap (lambda (v) (+ v 100)))) (wrap-macro 1))
; ;; ----------------------------------------------------------------


;; ----------------------------------------------------------------
; (define-syntax macro-or
;   (syntax-rules ()
;     ([macro-or] nil)
;     ([macro-or x xs ...]
;      (let ((v x))
;       (if v v (macro-or xs ...))))))

; (printm)
; (printm "first")

; (macro-or nil 2)

; (printm)
; (printm "second")

; (let ((v 2)) (macro-or nil v))
; (let ((v 2)) (and (macro-or nil v) (macro-or nil v)))

;; ----------------------------------------------------------------

; `(list ,(+ 1 2) 4)

; (define-syntax and (syntax-rules (then =>) ((and expr1 expr2 ...) (begin expr1 expr2 ...))))
; (and 1 2 3)
