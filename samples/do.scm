
(define-syntax do
  (syntax-rules ()
    ((do ((variable init step ...) ...)   ; Allow 0 or 1 step
        (test expression ...)
        command ...)
      (let loop ((variable init) ...)
        (if test
            (begin expression ...)
            (begin
              command ...
              (loop (do "step" variable step ...) ...)))))
    ((do "step" variable)
      variable)
    ((do "step" variable step)
      step)))

 (do ((vec (make-vector 5))
      (i 0 (+ i 1)))
     ((= i 5) vec)
   (vector-set! vec i i))

; new parser
; (let loop ((vec i make-vector 0) (vec i 5 0)) if (= i 5) begin vec begin (vector-set! vec i i))

; old parser
; (let loop (((vec i make-vector 0) (vec i 5 0))) (if (= i 5) (begin vec) (begin (vector-set! vec i i) (loop ((do "step" vec i +) (do "step" vec i i) (do "step" vec i 1))))))