
(let
    (
        (x 'a)
        (y 'a))
    (if
        (eq x y) 55 88))

(print
    (append '
        (3 3) 3))

(let loop
    (
        (vec
            (make-vector 5))
        (i 0))
    (if
        (= i 5)
        (begin vec)
        (begin
            (vector-set! vec i i)
            (loop
                (do "step" vec
                    (+ i 1))
                (do "step" i
                    (+ i 1) )))))


;; (do ((x 0 (+ x 1))) ((= x 3) (writeln 'done)) (writeln 'looping))
;; (do ((x 0 (+ x 1)) (y 666)) ((= x 3) (writeln 'done)) (writeln 'looping))

(do ((x 0 (+ x 1)) (y 666))
    ((= x 3) (writeln 'done))
    (writeln 'looping))

(do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
    (vector-set! vec i i))

;; (do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))

(do ([x 6 (- x 1)]
     [acc 1])
    ((zero? x) acc)
  (display x) (newline)
  (set! acc (* acc x)))