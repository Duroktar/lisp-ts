
(test-begin "spec")

(test-id '#4.1.1     28  (begin (define x 28) x))

(test-id '#4.1.2-a   'a  (quote a))
; (test-id '#4.1.2-b   '() (quote #(a b c)))
(test-id '#4.1.2-c   '(+ 1 2)  (quote (+ 1 2)))

(test-id '#4.1.2-d   'a  'a)
; (test-id '#4.1.2-e   '#(a b c)   '#(a b c))
(test-id '#4.1.2-f   '()   '())
(test-id '#4.1.2-g   '(+ 1 2)  '(+ 1 2))
(test-id '#4.1.2-h   '(quote a)  '(quote a))
(test-id '#4.1.2-i   '(quote a)  ''a)

(test-end)
