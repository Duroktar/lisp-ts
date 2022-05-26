(define (sum-to n)
  (if (= n 0)
      0
      (+ n (sum-to (- n 1)))))

(define (sum2-supp n acc)
  (if (= n 0)
      acc
      (sum2-supp (- n 1) (+ n acc))))

(define-syntax sum2
  (syntax-rules ()
    ((sum2 num acc)
     (sum2-supp num acc))
    ((sum2 num)
     (sum2 num 0))
  )
)

(define one-thousand 1000)
(define ten-thousand 10000)
(define one-million 1000000)

; (sum-to one-thousand)
(print (sum2 ten-thousand))
(print (sum2 one-million))
