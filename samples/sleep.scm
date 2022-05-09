(do
    (print 1)
)

(call/cc (lambda (throw) ((lambda (arg) (throw (print 1))))))

(do
    (print 3)
    (print 2)
    (print 1)
)


(call/cc (lambda (throw) ((lambda (arg) (throw (print 1)))
    (call/cc (lambda (throw) ((lambda (arg) (throw (print 2)))
        (call/cc (lambda (throw) ((lambda (arg) (throw (print 3))))))))))))

(call/cc (lambda (throw) ((lambda (arg) (throw (print 1)))
    (call/cc (lambda (throw) ((lambda (arg) (throw (print 2)))
        (call/cc (lambda (throw) (print 3)))))))))


(call/cc (lambda (throw)
      ((lambda (arg) (throw (print arg)))
      (call/cc (lambda (throw) (throw (print 1500)))))))

(call/cc (lambda (throw)
      ((lambda (arg) (throw (print (car arg))))
      (call/cc (lambda (throw) (throw 'arha))))))
      
(call/cc (lambda (throw)
      ((lambda (arg) (throw (print (car arg))))
      (call/cc (lambda (throw) (throw (= 5 5)))))))