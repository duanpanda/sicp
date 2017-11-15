; Exercise 1.46
;; (define (iterative-improve good-enough? improve)
;;   (define (try-it x)
;;     (if (good-enough? x)
;; 	x
;; 	(try-it (improve x))))
;;   (lambda (guess)
;;     (try-it guess)))
;or
(define (iterative-improve good-enough? improve)
 (define (try-it x)
   (if (good-enough? x)
	x
	(try-it (improve x))))
 try-it)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))

(define (average x y)
  (/ (+ x y) 2))