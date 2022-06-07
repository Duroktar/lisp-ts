(call-with-current-continuation
  (lambda (exit)
    (for-each (lambda (x)
                (if (negative? x)
                    (exit x)))
              '(54 0 37 -3 245 19))
    #t))

; (call-with-current-continuation (lambda (exit) (for-each (lambda (x) (if (negative? x) (exit x))) '(54 0 37 -3 245 19)) #t))

(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r
                  (lambda (obj)
                    (cond ((null? obj) 0)
                          ((pair? obj)
                           (+ (r (cdr obj)) 1))
                          (else (return #f))))))
          (r obj))))))

; (define list-length (lambda (obj) (call-with-current-continuation (lambda (return) (letrec ((r (lambda (obj) (cond ((null? obj) 0) ((pair? obj) (+ (r (cdr obj)) 1)) (else (return #f)))))) (r obj))))))

;; (list-length '(1 2 3 4))               ==>  4

;; (list-length '(a b . c))               ==>  #f
