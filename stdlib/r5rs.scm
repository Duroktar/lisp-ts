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
