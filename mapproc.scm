(define (my-map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (my-map proc (cdr items)))))

(define (my-multi-map proc . arg-items)
  (if (null? arg-items)
      '()
      (cons (apply (map car arg-items))
	    (apply my-multi-map
		   (cons proc (map cdr arg-items))))))

