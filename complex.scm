(define (make-complex-from-rect rl im) (list 'rect rl im))

(define (make-complex-from-polar mag ang) (list 'polar mag ang))

(define (tag complex) (car complex))

(define (contents complex) (cdr complex))

(define (real complex)
  (cond (eq? 'rect (tag complex)) (car (contents complex)))
  (cond (eq? 'polar (tag complex)) (* (car (contents complex)) (cos (cdr (contents complex)))))
  (else (error "unknown form of object")))


