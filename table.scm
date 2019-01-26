(define assoc-tag 'assoc)

(define (make-assoc) (list assoc-tag ()))

(define (assoc-set! assoc key value)
  (set-cdr! assoc (cons (list key value) (contents-of assoc))))

(define (contents-of assoc) (cdr assoc))

(define (assoc-get assoc key)
  (cond ((null? (car assoc)) #f)
	((eq? (car assoc) assoc-tag) (assoc-get (contents-of assoc) key))
	((eq? (caar assoc) key) (cadar assoc))
	(else (assoc-get (cdr assoc) key))))

;test associative lists
(define a1 (make-assoc))
(assoc-set! a1 1 1)
(assoc-set! a1 2 4)
(assoc-set! a1 5 25)

(define table-tag 'table)

(define (make-table size hashfunc)
  (let ((contents (make-vector 10 (make-assoc))))
    (list table-tag size hashfunc contents)))

(define (size-of table) (cadr table))

(define (hashfunc-of table) (caddr table))

(define (table-contents-of table) (cadddr table))

(define (bucket-at table index) (vector-ref (table-contents-of table) index))

(define (set-bucket-at table index value) (vector-set! (table-contents-of table) index value))

(define (index-of table key) ((hashfunc-of table) key (size-of table)))

(define (table-set table key value)
  (set-bucket-at
   table
   (index-of table key)
   (cons (list key value) (bucket-at table (index-of table key)))))

(define (table-ref table key)
  (assoc-get (bucket-at table (index-of table key)) key))

(define (hash1 key size)
  (cond (number? key) (modulo key size)))

(define t1 (make-table 10 hash1))
(table-set t1 1 2)

