;;;----------------------------------------------------------------------------

;; The read procedure.

(define (read)
  (let ((c (peek-char-non-whitespace)))
    (cond ((< c 0)
           c)
          ((eqv? c 40) ;; #\(
           (read-char) ;; skip "("
           (read-list))
          ((eqv? c 35) ;; #\#
           (read-char) ;; skip "#"
           (let ((c (peek-char)))
             (cond ((eqv? c 102) ;; #\f
                    (read-char) ;; skip "f"
                    #f)
                   ((eqv? c 116) ;; #\t
                    (read-char) ;; skip "t"
                    #t)
                   ((or (eqv? c 120) (eqv? c 88)) ;; #\x or #\X
                    (read-char) ;; skip "x"
                    (let ((c (peek-char)))
                      (if (eqv? c 45)
                          (begin
                            (read-char)
                            (read-hex 0))
                          (- 0 (read-hex 0)))))
                   (else ;; assume it is #\(
                    (list->vector (read))))))
          ((eqv? c 39) ;; #\'
           (read-char) ;; skip "'"
           (cons 'quote (cons (read) '())))
          ((eqv? c 34) ;; #\"
           (read-char) ;; skip """
           (list->string (read-chars '())))
          (else
           (read-char) ;; skip first char
           (let ((s (list->string (cons c (read-symbol)))))
             (let ((n (string->number s)))
               (or n
                   (string->symbol s))))))))

(define (read-hex accu)
  (let ((c (peek-char)))
    (if (and (< 47 c) (< c 58))
        (begin
          (read-char)
          (read-hex (- (* 16 accu) (- c 48))))
        (if (and (< 64 c) (< c 71))
            (begin
              (read-char)
              (read-hex (- (* 16 accu) (- c 55))))
            (if (and (< 96 c) (< c 103))
                (begin
                  (read-char)
                  (read-hex (- (* 16 accu) (- c 87))))
                accu)))))

(define (read-list)
  (let ((c (peek-char-non-whitespace)))
    (if (eqv? c 41) ;; #\)
        (begin
          (read-char) ;; skip ")"
          '())
        (let ((first (read)))
          (cons first (read-list))))))

(define (read-symbol)
  (let ((c (peek-char)))
    (if (or (eqv? c 40) ;; #\(
            (eqv? c 41) ;; #\)
            (< c 33)) ;; whitespace or eof?
        '()
        (begin
          (read-char)
          (cons c (read-symbol))))))

(define (read-chars lst)
  (let ((c (read-char)))
    (cond ((eof-object? c)
           '())
          ((eqv? c 34) ;; #\"
           (reverse lst))
          ((eqv? c 92) ;; #\\
;;           #; ;; no support for \n in strings
;;           (read-chars (cons (read-char) lst))
           ;#; ;; support for \n in strings
           (let ((c2 (read-char)))
             (read-chars
              (cons (cond
                     ;#; ;; support for \n in strings
                     ((eqv? c2 110) 10) ;; #\n
                     ;#; ;; support for \r in strings
                     ((eqv? c2 114) 13) ;; #\r
                     ;#; ;; support for \t in strings
                     ((eqv? c2 116) 9)  ;; #\t
                     (else          c2))
                    lst))))
          (else
           (read-chars (cons c lst))))))

(define (peek-char-non-whitespace)
  (let ((c (peek-char)))
    (if (eof-object? c) ;; eof?
        -1
        (if (< 32 c) ;; above #\space ?
            (if (eqv? c 59) ;; #\;
                (skip-comment)
                c)
            (begin
              (read-char)
              (peek-char-non-whitespace))))))

(define (skip-comment)
  (let ((c (read-char)))
    (if (< c 0) ;; eof?
        c
        (if (eqv? c 10) ;; #\newline
            (peek-char-non-whitespace)
            (skip-comment)))))

;;;----------------------------------------------------------------------------

;; The write procedure.

(define (write o)
  (cond ((string? o)
         (putchar 34)
         (write-chars (string->list o) #t)
         (putchar 34))
        (else
         (display o))))

(define (display o)
  (cond ((not o)
         (putchar2 35 102)) ;; #f
        ((eqv? o #t)
         (putchar2 35 116)) ;; #t
        ((null? o)
         (putchar2 40 41)) ;; ()
        ((pair? o)
         (putchar 40)  ;; #\(
         (write (car o))
         (write-list (cdr o))
         (putchar 41)) ;; #\)
        ((symbol? o)
         (display (symbol->string o)))
        ((string? o)
         (write-chars (string->list o) #f))
        ((vector? o)
         (putchar 35) ;; #\#
         (write (vector->list o)))
        ((procedure? o)
         (putchar2 35 112)) ;; #p
        (else
         ;; must be a number
         (display (number->string o)))))

(define (write-list lst)
  (if (pair? lst)
      (begin
        (putchar 32) ;; #\space
        (if (pair? lst)
            (begin
              (write (car lst))
              (write-list (cdr lst)))
            #f)) ;; writing dotted pairs is not supported
      #f))

(define (write-chars lst escape?)
  (if (pair? lst)
      (let ((c (car lst)))
        (putchar
         (cond ((not escape?)
                c)
               ;#; ;; support for \n in strings
               ((eqv? c 10) ;; #\newline
                (putchar 92) ;; #\\
                110)         ;; #\n
               ;#; ;; support for \r in strings
               ((eqv? c 13) ;; #\return
                (putchar 92) ;; #\\
                114)         ;; #\r
               ;#; ;; support for \t in strings
               ((eqv? c 9) ;; #\tab
                (putchar 92) ;; #\\
                116)         ;; #\t
               ((or (eqv? c 34) ;; #\"
                    (eqv? c 92)) ;; #\\
                (putchar 92) ;; #\\
                c)
               (else
                c)))
        (write-chars (cdr lst) escape?))
      #f))

(define write-char putchar)

(define (newline)
  (putchar 10))

(define (putchar2 c1 c2)
  (putchar c1)
  (putchar c2))

(define eof -1)
(define eof-object? (lambda (obj) (eqv? obj eof)))

(define empty -2)
(define buffer empty)

(define read-char (lambda ()
  (let ((c buffer))
    (if (eqv? c eof)
        c
        (read-char-aux
         (if (eqv? c empty)
             (getchar)
             c))))))

(define read-char-aux (lambda (c)
  (set! buffer c)
  (if (eqv? c eof)
      c
      (begin
        (set! buffer empty)
        c))))

(define (peek-char)
  (let ((c (read-char)))
    (set! buffer c)
    c))

(define repl (lambda ()
  (putchar2 62 32) ;; #\> and space
  (let ((expr (read)))
    (if (eof-object? expr)
        (newline)
        (begin
          (print expr)
          ; (write (eval expr))
          (newline)
          (repl))))))

(repl)
