(define stack-tag 'stack)

(define (make-stack) (list stack-tag ()))

(define (stack? stack) (eq? stack-tag (car stack)))

(define (top-stack stack)
  (cond ((not (stack? stack)) (error "not a stack"))
	((empty-stack? stack) (error "stack underflow - top"))
	(else (cadr stack))))

(define (push-stack stack element) (set-cdr! stack (cons element (cdr stack))))

(define (pop-stack stack)
  (if (empty-stack? stack)
      (error "stack underflow - pop")
      (let ((value (top-stack stack)))
	(set-cdr! stack (cddr stack))
	value)))

(define (empty-stack? stack) (null? (cadr stack)))
(define s (make-stack))
(push-stack s 1)
(push-stack s 2)
