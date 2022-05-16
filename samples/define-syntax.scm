
; (define-syntax case
;     (syntax-rules (else)
;       ([case (key ...)
;           clauses ...]
;         (let ([atom-key (key ...)])
;           (case atom-key clauses ...)))
;       ((case key
;           (else result1 result2 ...))
;         (begin result1 result2 ...))
;       ([case key
;           ((atoms ...) result1 result2 ...)]
;         (if [memv key '(atoms ...)]
;           (begin result1 result2 ...)))
;       ([case key
;           ((atoms ...) result1 result2 ...)
;           clause clauses ...]
;         (if [memv key '(atoms ...)]
;             (begin result1 result2 ...)
;             (case key clause clauses ...)))))

(printn "Result:"
  (case (* 2 3)
    ((2 3 5 7) 'prime)
    ((1 4 6 8 9) 'composite)))
