
import { env } from "./globals";
import { Lisp } from "./lib/lisp";

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

Lisp.exec(`
  (do ((i 0 (+ i 2)))
    ((>= i 5) (print i) (print "Done!"))
  (print i))
`, env);
