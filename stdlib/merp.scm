(load-from-library "pretty-print.scm")

(writeln (let ((a 0))
  (let ((f (lambda () a)))
    (let ((a 1))
      (f)))))

(writeln (let ((a 0))
  (let ((f (lambda () a)))
    (fluid-let ((a 1))
      (f)))))

(display (pp '(let ((a 0)) (let ((f (lambda () a))) (let ((a 1)) (f))))))
(newline)
