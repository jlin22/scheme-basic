(define (sum-expr? e) (and (pair? e) (eq? (car e) '+)))

(define (print e)
  (display e)
  (display "\n")
  )

(display "\n")
(print (sum-expr? (list '+ 'x 3)))
(print (not (sum-expr? 'x)))

(define (variable? e) (and (not (pair? e)) (symbol? e)))

(define (prod-expr? e) (and (pair? e) (eq? (car e) '*)))

(define (make-sum a1 a2)
  (cond ((number? a1)
      (if (number? a2)
	  (+ a1 a2)
	  (list '+ a1 a2)))
	((number? a2)
	 (list '+ a2 a1))
	(else (list '+ a1 a2))))

(define (addend sum) (cadr sum))

(define (augend sum) (caddr sum))

(define (multiplier prod) (cadr prod))

(define (multiplicand prod) (caddr prod))

(define (make-product p1 p2)
  (cond ((and (number? p1) (number? p2)) (* p1 p2))
	((number? p1)
	 (if (= p1 1)
	     p2
	     (list '* p1 p2)))
	((number? p2)
	 (if (= p2 1)
	     p1
	     (list '* p2 p1)))
	(else (list '* p1 p2))))

(define (deriv expr var)
  (cond ((variable? expr) (if (eq? expr var) 1 0))
	((number? expr) 0)
	((sum-expr? expr) (make-sum (deriv (addend expr) var)
				    (deriv (augend expr) var)))
	((prod-expr? expr)
	 (make-sum (make-product (multiplier expr) (deriv (multiplicand expr) var))
		   (make-product (multiplicand expr) (deriv (multiplier expr) var))))
	(else
	 (error "unknown expr type" expr))
	))

(print (deriv (list '+ 'x 3) 'x))
(print (deriv (list '+ 'x 'x) 'x))

(print (deriv (list '* 'x 'x) 'x))

