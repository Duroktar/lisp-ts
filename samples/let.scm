
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
