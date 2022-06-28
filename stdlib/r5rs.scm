; R5RS, ST-LIB

; - 4.2 Derived expression types
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

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax and
  (syntax-rules ()
    ([and] #t)
    ([and test] test)
    ([and test1 test2 ...]
      (if test1 [and test2 ...] #f))))

(define-syntax or
  (syntax-rules ()
    ([or] #f)
    ([or test] test)
    ([or test1 test2 ...]
      (let ([x test1])
        (if x x (or test2 ...))))))

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

(define-syntax let*
  (syntax-rules ()
    ((let* ((n1 e1) (n2 e2) (n3 e3) ...) body ...)
      (let ((n1 e1))
        (let* ((n2 e2) (n3 e3) ...) body ...)))
    ((let* ((name expression) ...) body ...)
      (let ((name expression) ...) body ...))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((variable init) ...) body ...)
      ((lambda ()
        (define variable init) ...
        body ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((variable init step ...) ...)   ; Allow 0 or 1 step
        (test expression ...)
        command ...)
      (let loop ((variable init) ...)
        (if test
            (begin expression ...)
            (begin
              command ...
              (loop (do "step" variable step ...) ...)))))
    ((do "step" variable)
      variable)
    ((do "step" variable step)
      step)))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
      (let ((forced #f)
            (memo #f))
        (lambda ()
          (if forced
              memo
              (begin
                (set! memo expression)
                (set! forced #t)
                memo)))))))

;;  - 6. Standard procedures
;; - 6.1 Equivalence Predicates

(define (eq? a b) (eqv? a b))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))

(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (not x) (eqv? x #f))
(define (boolean? obj) (or (eqv? obj #t) (not obj)))

(define eq? eqv?)

(define (for-each proc lst)
  (if (pair? lst)
      (begin
        (proc (car lst))
        (for-each proc (cdr lst)))
      #f))

(define (append first . rest)
  (cond ((null? rest) first)
        ((null? first) (apply append rest))
        (else
          (cons (car first)
                (append (cdr first)
                        (apply append rest))))))

(define (list-ref lst i)
  (car (list-tail lst i)))

(define (list-set! lst i x)
  (set-car! (list-tail lst i) x))

(define (list-tail lst i)
  (if (< 0 i)
      (list-tail (cdr lst) (- i 1))
      lst))

(define (make-list k fill)
  (make-list-aux k fill '()))

(define (make-list-aux k fill lst)
  (if (< 0 k)
      (make-list-aux (- k 1) fill (cons fill lst))
      lst))

(define (memv x lst)
  (if (pair? lst)
      (if (eqv? x (car lst))
          lst
          (memv x (cdr lst)))
      #f))

(define memq memv)

(define (member x lst)
  (if (pair? lst)
      (if (equal? x (car lst))
          lst
          (member x (cdr lst)))
      #f))

(define (assv x lst)
  (if (pair? lst)
      (let ((couple (car lst)))
        (if (eqv? x (car couple))
            couple
            (assv x (cdr lst))))
      #f))

(define (assq x lst) (assv x lst))

(define (assoc x lst)
  (if (pair? lst)
      (let ((couple (car lst)))
        (if (equal? x (car couple))
            couple
            (assoc x (cdr lst))))
      #f))

(define (remainder x y)
  (- x (* y (quotient x y))))

(define (modulo x y)
  (let ((q (quotient x y)))
    (let ((r (- x (* y q))))
      (if (eqv? r 0)
          0
          (if (eqv? (< x 0) (< y 0))
              r
              (+ r y))))))

(define (sub1 x) (- x 1))

(define (remainder x y)
  (- x (* y (quotient x y))))

(define (modulo x y)
  (let ((q (quotient x y)))
    (let ((r (- x (* y q))))
      (if (eqv? r 0)
          0
          (if (eqv? (< x 0) (< y 0))
              r
              (+ r y))))))

(define (expt x y)
  (if (eqv? y 0)
      1
      (let ((t (expt (* x x) (quotient y 2))))
        (if (odd? y)
            (* x t)
            t))))

(define (force promise) (promise))
