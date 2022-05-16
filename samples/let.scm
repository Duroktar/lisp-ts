
(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
      (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
        body1 body2 ...)
      (let ((name1 val1))
        (let* ((name2 val2) ...)
          body1 body2 ...)))))

(let ((x 2) (y 3))
  (let* ((x 7)
        (z (+ x y)))
    (* z x)))

(print
  (let ((x 2) (y 3))
    (let* ((x 7)
          (z (+ x y)))
      (* z x)))
) ; => 70
