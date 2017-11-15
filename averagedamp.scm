; Exercise 1.45
(define (average x y)
  (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(define (cube x) (* x (square x)))

(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (cube y))))
	       1.0))

(define (fifth-root x)
  (fixed-point ((repeated average-damp 2)
		(lambda (y) (/ x (square (square y)))))
	       1.0))

(define (sixth-root x)
  (fixed-point ((repeated average-damp 2)
		(lambda (y) (/ x (* y (square (square y))))))
	       1.0))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2))))
		(lambda (y) (/ x (fast-expt y (- n 1)))))
	       1.0))

(define (floor-log-base2-of-n i)
  (if (< i 2)
      0
      (+ 1 (floor-log-base2-of-n (/ i 2)))))
