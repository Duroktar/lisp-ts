(define-macro while (condition body)
  \`(let ((loop (lambda () (cond
            (,condition (begin ,body) (loop))))))))

(let ((loop (lambda () (cond
    ((> counter 0) (begin
        (display counter)
        (set! counter (- counter 1))
        (loop))
    )))))
  (display (loop)))

(cond
    ((> counter 0) (begin
        (display counter)
        (set! counter (- counter 1))))
    (#t (display "Done!"))
)

((lambda () (cond
    ((> counter 0) (begin
        (display counter)
        (set! counter (- counter 1))
        (display 'Doner)))
)))

(
    (lambda (loop) (loop))
    ((lambda () (cond
        ((> counter 0) (begin
            (display counter)
            (set! counter (- counter 1))
            (display 'Doner)))
    )) '())
)

(let ((loop (lambda () (cond
    ((> counter 0) (begin
        (display counter)
        (set! counter (- counter 1))
        (display 'Doner)))
)))) (loop))

(let ((i 55) (k 55)) (display (+ i k)))

((λ (loop) (loop) ((λ () (cond (((> counter 0) (begin ((display counter) (set! counter (- counter 1)) (display 'Doner))))))))))

((λ (i k) (display (+ i k)) (55 55)))

(let merp () (display (+ 6 6)))


; before:
(let my-loop ((x 1))
  (if (> x 10)
    (write "We're done!")
    (my-loop (+ x 1))))

; after:
((lambda ()
    (define my-loop (lambda (x)
                        (if (> x 10)
                            (write "We're done!")
                            (my-loop (+ x 1)))))
    (my-loop 1)))

((lambda () (begin
    (define my-loop (lambda (x)
                        (if (> x 10)
                            (print "We're done!")
                            (my-loop (+ x 1)))))
    (my-loop 1))))


(let my-loop ((x 1))
  (cond ((> x 10) (print "We're done!"))
    (else (my-loop (+ x 1)))))

(display (macroexpand '(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))))

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))

(define add4 ((lambda (x) (lambda (y) (+ x y))) 4))
