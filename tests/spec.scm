
(test-begin "spec")

(test '#4.1.1     28  (begin (define x 28) x))

(test '#4.1.2-a   'a  (quote a))
; (test '#4.1.2-b   '() (quote #(a b c)))
(test '#4.1.2-c   '(+ 1 2)  (quote (+ 1 2)))

(test '#4.1.2-d   'a  'a)
; (test '#4.1.2-e   '#(a b c)   '#(a b c))
(test '#4.1.2-f   '()   '())
(test '#4.1.2-g   '(+ 1 2)  '(+ 1 2))
(test '#4.1.2-h   '(quote a)  '(quote a))
(test '#4.1.2-i   '(quote a)  ''a)

; (test '#4.2.2-a 6
;   (let ((x 2) (y 3)) (* x y)))

; (test '#4.2.2-b 35
;   (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))

; (test '#4.2.2-c 70
;   (let ((x 2) (y 3))
;     (let* ((x 7)
;           (z (+ x y)))
;       (* z x))))

; (begin
;   (defun wrap (v) (list v))

;   (define-syntax wrap-macro
;     (syntax-rules ()
;       ([wrap-macro v]
;       (wrap v))))

;   (wrap-macro 1)

;   (test "Referential Transparency"  1
;     (let ((wrap (lambda (v) (+ v 100)))) (wrap-macro 1))))

(test-end)
