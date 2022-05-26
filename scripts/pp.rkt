#lang racket/base

(require racket/pretty)

(pretty-print 
  '((lambda () ((define even? (lambda (n) (if (zero? n) #t (odd? (- n 1))))) (define odd? (lambda (n) (if (zero? n) #f (even? (- n 1)))))) (list (even? 1000) (even? 1001) (odd? 1000))))
  
)
