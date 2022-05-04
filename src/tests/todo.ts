
namespace Testing {

  // Lisp.exec(`
  //   ;    (defun cadr         (x) (car (cdr x)))
  //   ; -> (label cadr (lambda (x) (car (cdr x))))
  //   (print (eval '(defun. capr (x y) (eq x y)) '((a aVar))))
  // `, Runtime.env)

  // Lisp.exec(`(debugn 'yppp (call/cc (lambda (throw) (eq 'm (throw 'hello)))))`, Runtime.env)

  // Lisp.exec("(print (let ((a 3) (b 2)) (* a b b b)))", Runtime.env)

  // Lisp.exec("(print (eq 'x 'x))", Runtime.env)
  // Lisp.exec("(print (eq 'x 'y))", Runtime.env)

  // Lisp.exec("(print (list 'a 'a 'a))", Runtime.env)
  // Lisp.exec("(print (list 'a 'a (list 'a 'a)))", Runtime.env)

  // Lisp.exec("(print (eq 'x 'x))", Runtime.env)
  // Lisp.exec("(print (cond ('#t 'x)))", Runtime.env)

  // Lisp.exec("(print (evlis '(x x x) '((x cat))))", Runtime.env)

  // Lisp.exec("(print (eval '(eq 'a 'a) '((a aVar))))", Runtime.env)
  // Lisp.exec("(print (eval '(list 'a 'a 'a) '((a aVar))))", Runtime.env)
  // Lisp.exec("(print (eval '(list 'a 'a (list 'a 'a)) '((a aVar))))", Runtime.env)

  // Lisp.exec("(print (eval '(eq a a) '((a aVar))))", Runtime.env)
  // Lisp.exec("(print (eval '(eq 'a 'a) '()))", Runtime.env)
  // Lisp.exec("(print (eval '(eq 'a 'b) '()))", Runtime.env)

  // Utils.print(Lisp.exec(`
  //   (eval '((label cadr (lambda (x) (car (cdr x)))) '(fst snd)) '()))
  // `, Runtime.env))

  // Utils.print(Lisp.exec(`
  //   (eval
  //     '(
  //       (label cadr (lambda (x) (car (cdr x))))
  //       ((label swap (lambda (x)
  //         (cons (car (cdr x)) (cons (car x) '()))))

  //       '(m b)))
  //     '())
  // `, Runtime.env))

  // Utils.debugLog(Lisp.exec(`
  //   (eval
  //     '(
  //       (label cadr (lambda (x) (car (cdr x))))
  //       ((label swap (lambda (x)
  //         (list (cadr x) (car x))))

  //       '(m b)))
  //     '())
  // `, Runtime.env))

  // Utils.debugLog(
  //   Lisp.read(`
  //     (eq ^a ^a)
  //   `)
  // )

  // Lisp.exec(`
  //   (print (eq ^a ^a))
  // `, Runtime.env)

  // Lisp.exec(`
  //   (print 3)
  // `, Runtime.env)

  // Lisp.exec(`
  //   (print (macroexpand '(if '(eq a b) 3 5)))
  // `, Runtime.env)

  // Lisp.exec(`
  //   (print (macroexpand '(let
  //     ((x 'a) (y 'a))
  //     (print (if (eq x x) 55 88))
  //   )))
  // `, Runtime.env)

  // Lisp.exec(`
  //   (define-macro is () '(= x y))
  // `, Runtime.env)

  // Lisp.exec(`
  //   (macroexpand '(is 2 3))
  // `, Runtime.env)

  // Lisp.exec(`
  //   (let
  //     ((x 3) (y 3))
  //     (print (is))
  //   )
  // `, Runtime.env)

  // Lisp.exec(`
  //   (let
  //     ((x 'a) (y 'a))
  //     (print (is))
  //   )
  // `, Runtime.env)

}
