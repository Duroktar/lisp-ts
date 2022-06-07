
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
      ((lambda (name ...) body1 body2 ...)
      val ...))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
          (if temp result temp)))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))


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
