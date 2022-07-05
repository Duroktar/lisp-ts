(load-from-library "r5rs.scm")

; (print (cond ((> 3 2) 'greater) ((< 3 2) 'less)))
; (cond ((> 3 2) 'greater) ((< 3 2) 'less))

(define (eq? a b) (eqv? a b))

(define (equal? a b) (cond
  ([list? a]
    (if (eqv? (car a) (car b))
        (equal? (cdr a) (cdr b))
        #f))
  (else (eqv? a b))))

(writeln (equal? (cons 1 3) (cons 1 3)))
