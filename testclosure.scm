(define (total-valid items)
  (define (is-valid? item)
    item)
  (define (total-valid-iter items total)
    (if (null? items)
	total
	(total-valid-iter (cdr items)
			  (if (is-valid? (car items))
			      (+ total 1)
			      total))))
  (total-valid-iter items 0))

(define (for-each proc items)
  (if (null? items)
      false
      (begin (proc (car items))
	     (for-each proc (cdr items))))
  'done)

(define (tv2 items)
  (let ((total 0))
    (define (is-valid? item) item)
    (define (tv2-recursive items)
      (cond ((null? items) 0)
	    ((is-valid? (car items)) (set! total (+ total 1))
				     (tv2-recursive (cdr items)))
	    (else (tv2-recursive (cdr items)))))
    (tv2-recursive items)
    total))

(define (tv3 items)
  (let ((total 0))
    (define (is-valid? item) item)
    (define (proc x)
      (if (is-valid? x) (set! total (+ total 1))))
    (for-each proc items)
    total))

(total-valid '(1 #t #f "good"))
;; 3

(tv2 '(1 #t #f "good"))
;; 3

(tv3 '(1 #t #f "good"))
;; 3