
import { env } from "./globals";
import { Lisp } from "./lib/lisp";
import { print, toString } from "./utils";
// import { print } from "./utils";

// Lisp.exec(`
//   (defun eval-expr (expr env)
//     (cond
//       ((atom expr) (env expr))
//       ((eq (car expr) 'lambda)
//         (lambda (arg)
//           (eval-expr caddr (lambda (y)
//                             (if (eq (cadr expr) y)
//                                 arg
//                                 (env y))
//             )))))
//       ('#t (
//         (eval-expr (car expr) env)
//         (eval-expr (cadr expr) env)))
//     ))
// `, env)
// Lisp.exec(`(print (eval-expr '((lambda (x y) y) (3 5)) (lambda (y) y)))`, env)
// Lisp.exec(`
//   (call/cc (lambda (throw)
//     ((lambda arg (throw (print 'Finished)))
//     (call/cc (lambda (throw) (throw (print 1500)))))))`, env)
// Lisp.exec(`
// (do
//   (print 1)
// )`, env)
// Lisp.exec(`
// (print (inspect '(do
//   (print 'Sleeping)
//   (sleep 1)
//   (print 'Done)
// )))`, env)
// Lisp.exec(`
// (do
//   (print 'Before)
//   (sleep 1000)
//   (print 'After)
// )`, env)
// Utils.print(Lisp.expand(Lisp.read(`
// (async
//   (print 'Before)
//   (sleep 1000)
//   (print 'After)

// )`)))

// Lisp.exec(`
//   (call/cc (((lambda (throw) ((lambda (arg) (print 'Done'))) 'arg))
//     (call/cc (lambda (throw) (lambda (arg) (sleep 1500 throw))))))`
//   , env)
// Lisp.exec(`
//   (do
//     (print 3)
//     (print 2)
//     (print 1)
//   )`
// , env)
// Lisp.exec(`(print (= 5 5))`
//   , env)
// Utils.print(Lisp.exec(`
//   (call/cc (lambda (throw)
//     (throw 'hello)
//   ))`, env))
// try {
//   Lisp.read(`
//   (begin
//     (print 'Start)
//     (= 4 "asdfd)
//     (print 'Done)
//   )`)
// } catch (err: any) {
//   console.error(err.formattedError)
//   process.exit(1)
// }

// Lisp.exec(`
//   (do ((i 0 (+ i 2)))
//     ((>= i 5) (print i) (print "Done!"))
//   (print i))
// `, env);

Lisp.exec(`
  (define counter 10)
`, env);

// print(Lisp.parse(`
//   (while (> counter 0)
//     (display counter)
//     (newline)
//     (set! counter (- counter 1)))
// `, env));

// Lisp.exec(`
//   (while (> counter 0)
//     (display counter)
//     (set! counter (- counter 1)))
// `, env)

// Lisp.exec(`
//   ((lambda () (cond
//     ((> counter 0) (begin
//         (display counter)
//         (set! counter (- counter 1))
//         (display counter)))
//   )) '())
// `, env)

// Lisp.exec(`
// (let ((loop (lambda () (cond
//   ((> counter 0) (begin
//       (display counter)
//       (set! counter (- counter 1))
//       (display 'Doner)))
// )))) (loop))
// `, env)

// print(Lisp.parse(`
// (let ((loop (lambda () (cond
//   ((> counter 0) (begin
//       (display counter)
//       (set! counter (- counter 1))
//       (display 'Doner)))
// )))) (loop))
// `, env))

// Lisp.exec(`
//   (let ((i 55) (k 55)) (display (+ i k)))
// `, env)

// print(Lisp.parse(`
//   (let my-loop ((x 1))
//       (if (> x 10)
//         (print "We're done!")
//         (my-loop (+ x 1))))
// `, env))

// Lisp.exec(`
//   (let my-loop ((x 1))
//     (if (> x 10)
//       (print "We're done!")
//       (my-loop (+ x 1))))
// `, env)

// Lisp.exec(`
//   (define add4 ((lambda (x) ((lambda (y) (+ x y)))) 4))
// `, env)

const EXPECTED = '(define add4 ((lambda (x) (lambda (y) (+ x y))) 4))';
const ACTUAL = toString(Lisp.parse(`
(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
`, env), false, 'lambda');

console.log('EXPECT:')
console.log(EXPECTED)

console.log('ACTUAL:')
print(ACTUAL)

console.log('IS TRUE???', ACTUAL === EXPECTED)

Lisp.exec(`
  (define add4
    (let ((x 4))
      (lambda (y) (+ x y))))
`, env)

Lisp.exec(`
  (printn "result:" (add4 6))
`, env)
