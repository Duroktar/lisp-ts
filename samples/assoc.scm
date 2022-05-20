; (define assv (x lst)
;   (if (pair? lst)
;       (let ((couple (car lst)))
;         (if (eqv? x (car couple))
;             'hair
;             (assv x (cdr lst))))
;       #f))

; (test #f (assq (list 'a) '(((a)) ((b)) ((c)))))

; (test '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

; (assv 5 '((2 3) (5 7) (11 13)))

; (test "#1" 6 (let ((x 2) (y 3)) (* x y)))

(let ((x 2) (y 3)) (* x y))

; (begin
;    (define *result* (try (lambda () (let ((x 2) (y 3)) (* x y)))))
;    (define *expected* 6)
;    (define *test-id* (if (not (eq? '"#1" '6)) (inspect "#1") " "))
;    (incr-total)
;    (if (equal? *expected* (cadr *result*))
;      (begin (if *verbose-test* (prints *test-id* "Passed...")) (incr-passed))
;      (begin
;        (incr-failed)
;        (if *verbose-test*
;          (begin
;            (newline)
;            (prints "!!! Failure !!!")
;            (if (not (eq? '"#1" '6))
;              (prints (string-pad-end " - Test Name:" 16) *test-id*))
;            (prints
;             (string-pad-end " - Expression:" 16)
;             (inspect '(let ((x 2) (y 3)) (* x y))))
;            (prints (string-pad-end " - Expected:" 16) (inspect *expected*))
;            (prints
;             (string-pad-end " - Actual:" 16)
;             (inspect (cadr *result*))))))))