; R5RS, ST-LIB

; - 4.2 Derived expression types
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
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
    ((let ((name val) ...) body1 body2 ...)
      ((lambda (name ...) body1 body2 ...)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
      (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
        body1 body2 ...)
      (let ((name1 val1))
        (let* ((name2 val2) ...)
          body1 body2 ...)))))

;; from: https://stackoverflow.com/questions/2835582/what-if-any-is-wrong-with-this-definition-of-letrec-in-scheme
(define-syntax letrec
  (syntax-rules ()
    ((letrec ((name val) ...) body bodies ...)
     ((lambda ()
       (define name val) ... body bodies ...)))))

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

(define (map proc lst)
  (if (pair? lst)
      (cons (proc (car lst)) (map proc (cdr lst)))
      '()))

(define (for-each proc lst)
  (if (pair? lst)
      (begin
        (proc (car lst))
        (for-each proc (cdr lst)))
      #f))

; (define list x x)

(define (append lst1 lst2)
  (if (pair? lst1)
      (cons (car lst1) (append (cdr lst1) lst2))
      lst2))

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

(define assq (x lst) (assv x lst))

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
