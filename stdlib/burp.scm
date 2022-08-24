(define-syntax quasiquote (syntax-rules (unquote unquote-splicing)
  (`,expr                 expr)
  (`(,@first . rest)      (append first `rest))
  (`(first . rest)        (cons `first `rest))
  (`#(,@first rest ...)   (list->vector `(,@first rest ...)))
  (`#(expr ...)           (list->vector `(expr ...)))
  (`expr                  'expr)))

(define L (list 1 2 3))

(display `(,@L))
(newline)
(display `(,L))
(newline)
