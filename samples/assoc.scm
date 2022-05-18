; (define assv (x lst)
;   (if (pair? lst)
;       (let ((couple (car lst)))
;         (if (eqv? x (car couple))
;             'hair
;             (assv x (cdr lst))))
;       #f))

(test #f (assq (list 'a) '(((a)) ((b)) ((c)))))

(test '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

(assv 5 '((2 3) (5 7) (11 13)))
