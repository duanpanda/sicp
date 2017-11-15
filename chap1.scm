; 1.1.7 Square Roots by Newton's Method
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

; Exercise 1.5 Applicative-order evaluation or normal-order evaluation
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))

; 1.2.2 Tree Recursion
; Fibonacci: Recursive Procedure
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (= (fib (- n 1))
		 (fib (- n 2))))))
; Fibonacci: Iterative Procedure
(define (fib n)
  (fib-iter 1 0 n))
; a <-- a + b
; b <-- a
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; Counting change.
; dollar      100
; half-dollar 50
; quarter     25
; dime        10
; nickel      5
; penny       1
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))
(count-change 100)

; Exercise 1.11
; Recursive Procedure
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
; Iterative Procedure
(define (f n)
  (fi 2 1 0 n))
; a <-- a + 2b + 3c
; b <-- a
; c <-- b
(define (fi a b c count)
  (if (= count 0)
      c
      (fi (+ a (* 2 b) (* 3 c)) a b (- count 1))))

; Exercise 1.12 Pascal's triangle
; pas(n,k)
; if (k=1) or (k=n), then pas(n,k) = 1
; else pas(n,k) = pas(n-1,k-1) + pas(n-1,k)
(define (pas n k)
  (if (or (= k 1) (= k n))
      1
      (+ (pas (- n 1) (- k 1))
	 (pas (- n 1) k))))

; Exercise 1.13
Observe
Φ^2 = ((1 + √5) / 2)^2 = (1 + 5 + 2√5) / 4 = (6 + 2√5) / 4 = (3 + √5) / 2
    = Φ + 1.
First we show: Fib(n) = (Φ^n - Ψ^n) / √5.

Fib(0) = (Φ^0 - Ψ^0) / √5 = 0.
Fib(1) = (Φ^1 - Ψ^1) / √5 = [(1 + √5) / 2 - (1 - √5) / 2] / √5 = √5 / √5 = 1.
Assume Fib(k) = (Φ^k - Ψ^k) / √5, substitute k by (n-1) or (n-2) respectively,
using induction and the definition of the Fibonacci, we can deduce:
Fib(n) = Fib(n-1) + Fib(n-2)
       = (Φ^(n-1) - Ψ^(n-1)) / √5 + (Φ^(n-2) - Ψ^(n-2)) / √5
       = (Φ*Φ^(n-2) + Φ^(n-2) / √5 - (Ψ*Ψ^(n-2) + Ψ^(n-2)) / √5
       = ((Φ+1) * Φ^(n-2) - (Ψ+1) * Ψ^(n-2)) / √5
       = (Φ^2 * Φ^(n-2) - Ψ^2 * Ψ^(n-2)) / √5
       = (Φ^n - Ψ^n) / √5.

∵ -1 < Ψ < 0
∴ -0.5 <  -1/√5 < Ψ^n/√5 < 1/√5 < 0.5
∴|Φ^n / √5 - Fib(n)| = |Ψ^n / √5| < 0.5
So Fib(n) is the closest integer to Φ^n / √5.

; Exercise 1.14
树的深度取决于最左边的子树，即(n 1)，它的深度是n+6，Θ(n)。
然后对于计算步数，也就是树的节点数，我们知道对于一个二叉树，树的节点数 = 左子树节点数 + 右子树节点数 + 1.
先来看 (n 1) 子树，设它的节点数是f(n), 而且总有，非叶节点左子树节点数为1
当 n=1，f(1) = 3
   n>1, f(n) = 1 + f(n-1) + 1 = f(n-1) + 2 = f(n-2) + 2*2 
             = f(n-(n-1)) + 2*(n-1) = 2n + 1
             = O(n)

再来看 (n 2) 子树，设它的节点数 g(n), 设 └n/5┘ = A 
g(n) = f(n) + g(n-5) + 1 = f(n) + f(n-5) + g(n-5*2) + 2
     = f(n) + ... + f(n-5*(A-1)) + g(n-5*A) + 2A
     = O(n^2)

依此类推，可以得出结论 (n 5) 的计算步数增长的阶为 Θ(n^5)。

; Exercise 1.15
(define (cube x) (* x x x))
(define (p x)(- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
(sine 12.15)
(sine a)

a) 12.15连除5次3小于0.1，所以是5次。
b) 可以看出每调用一次 p 过程，需要递归1次 sine ，空间加1，计算步数加2，关键是p的次数：
对于a，调用次数t，那么 a*3^(-t) < 0.1 , 即 10a < 3^t ==> lg(10a)/lg3 < t,
所以增长阶 空间和时间 都为 Θ(log a)。

; 1.2.4 Exponentiation

; Θ(n) steps, and Θ(n) space.
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; Θ(n) steps, and Θ(1) space.
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))

; Using successive squaring to compute exponentials in fewer steps.
; fast-expt grows logarithmically with n in both space and number of steps.
; Θ(log n) steps, Θ(log n) space.
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1)))))
(define (even? n)
  (= (remainder n 2) 0))

; Exercise 1.16
; Θ(log n) steps, Θ(1) space.
; Invariant quantity method example: a * b^n = Constant, a = 1, b = 2, n = 5,
;   a * b^n
; = 1 * 2^5
; = 2 * 2^4
; = 4 * 2^3
; = 8 * 2^2
; = 16 * 2^1
; = 32 * 2^0
; now a = 32.
(define (fast-expt b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter (square b)
				   (/ n 2)
				   a))
	(else (fast-expt-iter b
			      (- n 1)
			      (* a b)))))
; Example process of fast-expt:
; (fast-expt 2 7)
; (b n a)
; (2 7 1)
; (2 6 1*2) ==> (2 6 2)
; (2^2 6/2 2) ==> (4 3 2)
; (4 2 2*4) ==> (4 2 8)
; (4^2 2/2 8) ==> (16 1 8)
; (16 0 8*16) ==> (16 0 128)
; 128

; Exercise 1.17
; Θ(log b) steps, Θ(log b) space.
(define (fast-mult a b)
  (cond ((= b 0) 0)
	((even? b) (double (fast-mult a (halve b))))
	(else (+ a (fast-mult a (- b 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(define (double n) (+ n n))
(define (halve n) (/ n 2))

; Exercise 1.18
; Θ(log b) steps, Θ(log b) space.
; Invariable quantity: a * b + m = Constant
(define (fast-mult a b)
  (fast-mult-iter a b 0))
(define (fast-mult-iter a b m)
  (cond ((= b 0) m)
	((even? b) (fast-mult-iter (double a)
				   (halve b)
				   m))
	(else (fast-mult-iter a (- b 1) (+ m a)))))

; Exercise 1.19
;T(pq):(a,b)=>(bq+aq+ap, bp+aq)
;T(pq):(bq+aq+ap, bp+aq)=>((bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p,
;                          (bp+aq)p + (bq+aq+ap)q)
;      => (2bpq+2aqq+bqq+2apq+app, bpp+2apq+bqq+aqq)
;      => (b(2pq+qq)+a(2pq+qq)+a(qq+pp), b(qq+pp)+a(2pq+qq))
;q' = 2pq+qq
;p' = qq+pp
;
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q ) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; 1.2.5 Greatest Common Divisors
; Euclid's Algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Exercise 1.20
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (% 206 40)))
(gcd 40 (% 206 40))
(if (= (% 206 40) 0) ;<##  1> [remainder = 6]
    40
    (gcd (% 206 40) (% 40 (% 206 40))
(gcd (% 206 40) (% 40 (% 206 40)))
(if (= (% 40 (% 206 40)) 0)   ;<##  2> [4]
    (% 206 40)
    (gcd (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))))
(gcd (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))
(if (= (% (% 206 40) (% 40 (% 206 40))) 0) ;<## 4>[2]
    (% 40 (% 206 40))
    (gcd (% (% 206 40) (% 40 (% 206 40)))
	 (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))))
(gcd (% (% 206 40) (% 40 (% 206 40)))
     (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))))
(if (= (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))) 0) ;<## 7>[0]
    (% (% 206 40) (% 40 (% 206 40)))
    (gcd ...)
(% (% 206 40) (% 40 (% 206 40)))  ;<## 4>[2]

可以看出需要调用 remainder 共 1+2+4+7+4 = 18 次。

应用序计算过程如下：
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (% 206 40))) ;<##>
(gcd 40 6)
(if (= 6 0) 40 (gcd 6 (% 40 6)))      ;<##>
(gcd 6 4)
(if (= 4 0) 6 (gcd 4 (% 6 4)))        ;<##>
(gcd 4 2)
(if (= 2 0) 4 (gcd 2 (% 4 2)))        ;<##>
(gcd 2 0)
(if (= 0 0) 2 (gcd 0 (% 2 0)))
2
可以看出共需 4 次。

; 1.2.6 Example: Testing for primality
; Searching for divisors
; Θ(√n) steps
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
; The end test for find-divisor is based on the fact that if n is not prime it
; must have a divisor less than or equal to √n.  If d is a divisor of n, then so is n/d. But d and n/d cannot both be greater than √n.

; The Fermat test
; Θ(log n)
;
; Fermat's Little Theorem: If n is a prime number and a is any positive integer
; less than n, then a raised to the nth power is congruent to a modulo n.

; expmod computes the exponential of a number modulo another number.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))
;   (x * y) mod m
; = [(x mod m) * (y mod m)] mod m
;
; So if e is even, then
;   b^e mod m
; = (b^(e/2) mod m)^2 mod m
;
; This technique is useful because it means we can perform our computation
; without ever having to deal with numbers much larger than m.
;
; Compare Exercise 1.25.
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))	; obtain a random number [1, n-1]
(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

; 1.3.1 Procedures as Arguments

; Compute the sum of the integers from a through b.
(define (sum-intergers a b)
  (if (> a b)
      0
      (+ a (sum-intergers (+ a 1) b))))

; Compute the sum of the cubes of the integers in the given range.
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

; Compute 1/(1*3) + 1/(5*7) + 1/(9*11) + ...
; --> π/8 (converges to π/8 very slowly).
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

; Template procedure with common pattern.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; Compute the definite integral of a function f between the limits a and b.
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; 1.3.2 Lambda expressions
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integeral f a b dx)
  (* (sum f
	  (+ a (/ dx 2.0))
	  (lambda (x) (+ x dx))
	  b)
     dx))

; 1.3.3 Procedures as General Method

; Finding roots of equations by the half-interval method.
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

; Finding fixed points of functions
; f(x) = x, x is called a fixed point of the function f.
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

; not converge:
; (define (sqrt x)
;   (fixed-point (lambda (y) (/ x y))
; 	       1.0))

; Average Damping
; To find the square root of x,
; y^2 = x,
; y = x / y, but it does not converge.
;
; 2y = y + x/y,
; y = (1/2)(y + x/y) = f(y), <== Find the fixed point of f(y).
; The same method as that in Section 1.1.7.
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

; 1.3.4 Procedures as Returned Values
(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)
55

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

; y -> x/y^2
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

; Newton's method
; If x -> g(x) is a differentiable function, then a solution of the equation
; g(x) = 0 is a fixed point of the function x -> f(x) where
; f(x) = x - g(x) / Dg(x),
; and Dg(x) is the derivative of g evaluated at x.
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (cube x) (* x x x))
((deriv cube) 5)			; 75.00014999664018

; Express Newton's method as a fixed-point process:
(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x)
	  ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

; Abstractions and first-class procedures
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))

; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a (* x x))
       (* b x)
       c)))

; Exercise 1.41
(define (double proc)
  (lambda (x)
    (proc (proc x))))
; (((double (double double)) inc) 5) = 5 + 2^(2^2) = 5 + 2^4 = 21

; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
; ((compose square inc) 6) = 49

; Exercise 1.43
;(define (repeated f n)
;    (if (= n 1)
;        f
;        (repeated (compose f f) (- n 1))))
;or
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

; Exercise 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; Exercise 1.45
(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2))))
		(lambda (y) (/ x (fast-expt y (- n 1)))))
	       1.0))

; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (try-it x)
    (if (good-enough? x)
	x
	(try-it (improve x))))
  (lambda (guess)
    (try-it guess)))

; Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))