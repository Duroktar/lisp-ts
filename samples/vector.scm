(let ((v (make-vector 5)))
      (for-each
        (lambda (i) (vector-set! v i (* i i)))
        '(0 1 2 3 4))
      v)

(let ((v (list 1 2 3))) (display "first") (display "done"))