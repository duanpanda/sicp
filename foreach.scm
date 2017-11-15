;; Exercise 2.23
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
	     (for-each proc (stream-cdr items)))))