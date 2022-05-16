
import { env } from "./globals";
import * as Lisp from "./lib/lisp";
import { print } from "./utils";
// import { executeFile } from "./load";
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

// Lisp.exec(`
//   (let ((i 55) (k 55)) (display (+ i k)))
// `, env)

// Lisp.exec(`
//   (let my-loop ((x 1))
//     (if (> x 10)
//       (print "We're done!")
//       (my-loop (+ x 1))))
// `, env)

// Lisp.exec(`
//   (define add4
//     (let ((x 4))
//       (lambda (y) (+ x y))))
// `, env)

// Lisp.exec(`
//   (printn "result:" (add4 6))
// `, env)

// print(Lisp.parse(`
//   (while (> counter 0)
//     (display counter)
//     (set! counter (- counter 1)))
// `, env))

// Lisp.exec(`
//   (+ 1 1)
// `, env)

// Lisp.exec(`
//   (let my-loop ((x 1))
//     (if (> x 10)
//       (print "We're done!")
//       (my-loop (printr (+ x 1)))))
// `, env)

// Lisp.exec(`
//   (define counter 10)
// `, env);

// print(Lisp.parse(`
//   (while (> counter 0)
//     (display counter)
//     (set! counter (sub1 counter)))
// `, env))

// Lisp.exec(`
//   (while (> counter 0)
//     (display counter)
//     (set! counter (sub1 counter)))
// `, env)

// Lisp.exec(`
//   (print (cond
//     ((> 1 10)   "We're done!")
//     (else       (+ 1 1))))
// `, env)

// Lisp.exec(`
//   (let my-loop ((x 1))
//     (cond ((> x 10) (print "We're done!"))
//           (else (my-loop (printr (+ x 1))))))
// `, env)

// Lisp.exec(`
//   (let fac ((n 10))
//     (if (zero? n)
//         1
//         (* n (fac (sub1 n)))))
// `, env)

// print(Lisp.parse(`
//   (let fac ((n 10))
//     (if (zero? n)
//         1
//         (* n (fac (sub1 n)))))
// `, env))

// Lisp.execute('(load "samples/fac.scm")', env)

// Lisp.execute('(print (fac 12))', env)

// print(Lisp.parse('(and (= 2 2) (> 2 1))', env))
// print(Lisp.parse(`(and 1 2 'c '(f g))`, env))
// print(Lisp.execute(`(and 1 2 'c '(f g))`, env))

// print(Lisp.parse(`(and (= 2 2) (> 2 1))`, env))
// print(Lisp.parse(`(and 1)`, env))
// print(Lisp.parse(`(and 1 2)`, env))
// print(Lisp.parse(`(and 1 2 3)`, env))
// print(Lisp.parse(`(and 1 2 3 4)`, env))
// print(Lisp.parse(`(and 1 2 3 4 5)`, env))
// print(Lisp.parse(`(and 1 2 #f 4 5)`, env))

// print(["(and (= 2 2) (> 2 1)) =>", Lisp.execute(`(and (= 2 2) (> 2 1))`, env)])
// print(["(and) =>", Lisp.execute(`(and)`, env)])
// print(["(and 1) =>", Lisp.execute(`(and 1)`, env)])
// print(["(and 1 2) =>", Lisp.execute(`(and 1 2)`, env)])
// print(["(and 1 2 3) =>", Lisp.execute(`(and 1 2 3)`, env)])
// print(["(and 1 2 3 4) =>", Lisp.execute(`(and 1 2 3 4)`, env)])
// print(["(and 1 2 3 4 5) =>", Lisp.execute(`(and 1 2 3 4 5)`, env)])
// print(["(and 1 2 #f 4 5) =>", Lisp.execute(`(and 1 2 #f 4 5)`, env)])

// print(Lisp.parse(`(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))`, env))

// Lisp.execute('(load "samples/define-syntax.scm")', env)
Lisp.execute('(load "samples/let.scm")', env)
