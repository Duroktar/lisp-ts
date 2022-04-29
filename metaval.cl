(defun nil. (x)
  (eq x '()))

(defun and. (x y)
  (cond
    (x (cond (y '#t) ('#t '()) ) )
    ('#t '())))

(defun not. (x)
  (cond
    (x '())
    ('#t '#t)))

(defun append. (x y)
  (cond ((nil. x) y)
        ('#t (cons (car x) (append. (cdr x) y)))))

(defun pair. (x y)
  (cond ((and. (nil. x) (nil. y)) '())
        ((and. (not. (atom x)) (not. (atom y)))
          (cons (list  (car x) (car y))
                (pair. (cdr x) (cdr y))))))

(defun assoc. (x y)
  (cond ((eq (caar y) x) (cadar y))
        ('#t (assoc. x (cdr y)))))

(defun eval (e a)
  (cond
    ((atom e) (assoc. e a))
    ((atom (car e))
      (cond
        ((eq (car e) 'quote.) (cadr e))
        ((eq (car e) 'atom.)  (atom  (eval (cadr e)  a)))
        ((eq (car e) 'eq.)    (eq    (eval (cadr e)  a)
                                     (eval (caddr e) a)))
        ((eq (car e) 'car.)   (car   (eval (cadr e)  a)))
        ((eq (car e) 'cdr.)   (cdr   (eval (cadr e)  a)))
        ((eq (car e) 'cons.)  (cons  (eval (cadr e)  a)
                                     (eval (caddr e) a)))
        ((eq (car e) 'cond.)  (evcon (cdr e)))
        ('#t (eval (cons (assoc. (car e) a)
                         (cdr e))
                    a))))

    ((eq (caar e) 'label.)
      (eval (cons (caddar e) (cdr e))
            (cons (list (cadar e) (car e)) a)))

    ((eq (caar e) 'lambda.)
      (eval (caddar e)
              (append. (pair. (cadar e) (evlis. (cdr e) a))
                      a)))))






(defun eval (e a)
      (cond
        ((atom      e ) (assoc e a))
        ((atom (car e))
          (cond
            ((eq (car e) 'quote) (cadr e))
            ((eq (car e) 'atom)  (atom (eval (cadr  e) a)))
            ((eq (car e) 'eq)    (eq   (eval (cadr  e) a)
                                       (eval (caddr e) a)))
            ((eq (car e) 'car)   (car  (eval (cadr  e) a)))
            ((eq (car e) 'cdr)   (cdr  (eval (cadr  e) a)))
            ((eq (car e) 'cons)  (cons (eval (cadr  e) a)
                                       (eval (caddr e) a)))
            ((eq (car e) 'cond) (evcon (cdr e)))
            ('#t (eval (cons (assoc (car e) a)
                             (cdr e))
                        a))))

        ((eq (caar e) 'label)
          (eval (cons (caddar e) (cdr e))
                (cons (list (cadar e) (car e)) a)))

        ((eq (caar e) 'lambda)
          (eval (caddar e)
                  (append (pair (cadar e) (evlis (cdr e) a))
                          a)))))

(defun evcon. (c a)
  (cond
        ((eval (caar c) a) (eval (cadar c) a))
        ('#t (evcon. (cdr c) a))))

(defun evlis. (m a)
  (cond
        ((nil m) '())
        ('#t (cons (eval   (car m) a)
                   (evlis. (cdr m) a)))))









(label subst (lambda x y z)
  (cond ((atom z)
          (cond ((eq z y) x)
                  ('#t z)))
          ('#t (cons (subst x y (car z))
                      (subst x y (cdr z))))))

(
  (label subst (lambda (x y z)
                  (cond
                    ((atom z)
                      (cond (
                        (eq z y) x)
                        ('#t z)))
                    ((nil. z) z)
                    ('#t (cons (subst x y (car z))
                               (subst x y (cdr z)))))))
  (subst 'm 'b '(a b (a b c) d))
)

(
(label subst (lambda (x y z)
  (cond ((atom z)
    (cond ((eq z y) x)
           ('#t z)))
  ('#t (cons (subst x y (car z))
             (subst x y (cdr z)))))))

(subst 'm 'b '(a b (a b c) d))
)




((label cadr (lambda (x) (car (cdr x)))) '(m b))

(
  (label cadr (lambda (x) (car (cdr x))))
  ((label swap (lambda (x)
    (cons (car (cdr x)) (cons (car x) '()))))

  '(m b)))

((label cadr (lambda (x) (car (cdr x))) ((label swap (lambda (x) (cons (car (cdr x)) (cons (car x) '())))) '(m b) )))


((label cadr (lambda (x) (car (cdr x)))) '(x y))
((label swap (lambda (x) (list (car (cdr x)) (car x)))) '(x y))

(
      (label cadr (lambda (x) (car (cdr x))))
      ((label swap (lambda (x) (list (cadr x) (car x))))

      '(x y)))



(eval
  '(
    (label swap (lambda (x) (list (car (cdr x)) (car x))))
    '(m b))
  '((m mars) (b cat))
)










(define ADD3 (n) (+ n 3))

(ADD3 3)

// 6

