(do
    (print 1)
    (print 2)
)

(call/cc (((lambda (throw) ((lambda (arg) (print 'Done'))) 'arg))
    (call/cc (lambda (throw) (lambda (arg) (sleep 1500 throw))))))

(
    (lambda (cont) ((lambda (arg) (sleep 1500 cont)) 'arg))
    (lambda (cont) (lambda (arg) (cont (print 'Done'))))
)
