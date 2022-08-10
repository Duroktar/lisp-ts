(define (some-thing a b)
  (cond
    ((= 3 a) 'a)
    ((= 3 b) 'b))
    ((= b b) 'c)
    (else 'lol))

; (describe '(some-thing 2 3))
; (some-thing 2 3)
