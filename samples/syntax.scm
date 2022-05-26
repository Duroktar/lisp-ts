
(define-syntax macro-or
  (syntax-rules ()
    ([macro-or arg] (+ v arg))))

(macro-or 3)
