;; from: https://stackoverflow.com/questions/2835582/what-if-any-is-wrong-with-this-definition-of-letrec-in-scheme
(define-syntax letrec
  (syntax-rules ()
    ((letrec ((name val) ...) body bodies ...)
     ((lambda ()
       (define name val) ... body bodies ...)))))

; (print
;   (letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
;            (odd?  (lambda (n) (if (zero? n) #f (even? (- n 1))))))
;         (list
;           (even? 1000)
;           (even? 1001)
;           (odd?  1000))))

; ;; returns ->

; '((lambda ()
;     ((define even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
;      (define odd? (lambda (n) (if (zero? n) #f (even? (- n 1))))))
;     (list (even? 1000) (even? 1001) (odd? 1000))))


(letrec ([x (+ 2 2)] [y (+ 2 2)] [z (+ 2 2)]) (writeln (+ x x)))
