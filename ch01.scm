;; Exercise 1.1
;; 10
10

;; (+ 5 3 4)
12

;; (- 9 1)
8

;; (/ 6 2)
3

;; (+ (* 2 4) (- 4 6))
;; equals to (+ 8 -2)
6

;; (define a 3)
;; now a = 3

;; (define b (+ a 1))
;; b = 4

;; (+ a b (* a b))
;; equals to (+ 3 4 (* 3 4))
;; equals to (+ 3 4 12)
19

;; (= a b)
false

;; (if (and (> b a) (< b (* a b)))
;;     b
;;     a)
;; b > a is ture, b < a * b is true, so the result is b
4

;; (cond ((= a 4) 6)
;;       ((= b 4) (+ 6 7 a))
;;       (else 25))
16

;; (+ 2 (if (> b a) b a))
6

;; (* (cond ((> a b) a)
;;          ((< a b) b)
;;          (else -1))
;;    (+ a 1))
16


;; Exercise 1.2
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))


;; Exercise 1.3
(define (sum-of-square a b)
  (+ (square a) (square b)))

(define (foo a b c)
  (cond ((and (> a c) (> b c))
         (sum-of-square a b))
        ((and (> a b) (> c b))
         (sum-of-square a c))
        ((and (> b a) (> c a))
         (sum-of-square b c))))


;; Exercise 1.4
;; If b > 0, the result will be a + b
;; else a - b, so the behavior is a plus absolute value of b,
;; as you can see from the procedure name.

;; Exercise 1.5
;; In applicative order, (p) will be evaluated before test being
;; applied on operands. Evaluating (p) will cause a infinite 
;; loop, because the value of (p) is itself.
;; In normal order, operands won't be evaluated until their 
;; values are needed. (test 0 (p)) will be evaluated as 
;; (if (= 0 0) 0 (p)), (= 0 0) will be evaluated first,
;; thus the value of the if expression is 0.

;; Exercise 1.6
;; Again this is problem relating to the evaluation method of
;; intepreter. Here we assume the method is applicative order.
;; The operands will be evaluated first, (sqrt-iter guess x)
;; will be called infinitely, the procedure will be in a 
;; infinite recursive loop.

;; Exercise 1.7
(define (sqrt-iter guess last-guess x)
  (if (good-enough? guess last-guess)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

;; Good enough when change is less than 1%
(define (good-enough? guess last-guess)
  (< (/ (abs (- guess last-guess))
        last-guess)
     0.01))

(define (sqrt x)
  (sqrt-iter (improve 1.0 x) 1.0 x))

;; Exercise 1.8
;; Based on procedures in Exercise 1.7, just change the
;; improve procedure as follows.
(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

;; Exercise 1.9
(define (+ a b)
  (if (= a 0)
      (inc (+ (dec a) b))))

;; (+ 4 5) ------------------------------
;; (inc (+ 3 5))                        |
;; (inc (inc (+ 2 5)))                  |
;; (inc (inc (inc (+ 1 5))))            |
;; (inc (inc (inc (inc (+ 0 5)))))      |
;; (inc (inc (inc (inc 5))))            |
;; (inc (inc (inc 6)))                  |
;; (inc (inc 7))                        |
;; (inc 8)                              |
;; 9       <-----------------------------
;; 
;; recursive process

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; (+ 4 5)   -----
;; (+ 3 6)       |
;; (+ 2 7)       |
;; (+ 1 8)       |
;; (+ 0 9)       |
;; 9             V
;; 
;; iterative process


;; Exercise 1.10
(A 1 10)
;; 1024

(A 2 4)
;; 65535

(A 3 3)
;; 65535

(define (f n) (A 0 n))
;; 2n

(define (g n) (A 1 n))
;; 2^n

(define (h n) (A 2 n))
;; 2^(2^(2^(...2)) the number of 2 is determined by n


;; Exercise 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f n)
  (define (f-iter a b c count)
    (if (> count n)
        a
        (f-iter (+ a
                   (* 2 b)
                   (* 3 c))
                a
                b
                (+ count 1))))
  (if (< n 3)
      n
      (f-iter 2 1 0 3)))



;; Exercise 1.12
(define (pas-tri row col)
  (if (or (= col 1) 
          (= col row))
      1
      (+ (pas-tri (- row 1) (- col 1))
         (pas-tri (- row 1) col))))

;; Exercise 1.13
;; Too much to write it here, any questions please
;; contact fuqcool@gmail.com

;; Exercise 1.16
(define (expt b n)
  (expt-iter 1 b n))

(define (even? num)
  (= (remainder num 2) 0))

(define (expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (expt-iter a (square b) (/ n 2)))
        (else (expt-iter (* a b)
                         b
                         (- n 1)))))

;; Exercise 1.17
(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

;; Exercise 1.18
(define (* a b)
  (*-iter 0 a b))

(define (*-iter sum a b)
  (cond ((= b 0) sum)
        ((even? b) (*-iter sum (double a) (halve b)))
        (else (*-iter (+ sum a)
                      a
                      (- b 1)))))

;; Exercise 1.19
;; p'
(+ (square p)
   (square q))
;; q'
(+ (* 2 p q)
   (square q))

;; Exercise 1.21
199, 1999, 7

;; Exercise 1.22
(define (search-for-primes n)
  (search-prime n 3))

(define (search-prime n count)
  (cond ((= count 0) 
         (newline)
         (display "end of search"))
        ((prime? n) 
         (timed-prime-test n)
         (search-prime (+ n 1) (- count 1)))
        (else (search-prime (+ n 1) count))))

;; Since modern computers are much faster now, it's
;; hard to test elapsed time for small primes like 
;; 10000. So I choose larger ones for more obvious
;; results. The results are as follows.

;; 1000000007 *** .07
;; 1000000009 *** .07
;; 1000000021 *** .07000000000000006
;; end of search

;; 10000000019 *** .21000000000000008
;; 10000000033 *** .19999999999999996
;; 10000000061 *** .19999999999999996
;; end of search

;; 100000000003 *** .6500000000000004
;; 100000000019 *** .6299999999999999
;; 100000000057 *** .6399999999999997
;; end of search

;; 1000000000039 *** 2.0100000000000007
;; 1000000000061 *** 2.0199999999999996
;; 1000000000063 *** 2.0199999999999996
;; end of search

;; We can see that when size grows by 10 times, 
;; the time needed to test prime number grows by
;; nearly 3 times. And when size grows by 100 times,
;; the elapsed time grows by nearly 9 times. The
;; data above supports the sqrt(10) prediction well.

;; Exercise 1.23

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

;; 1000000007 *** .03999999999999915
;; 1000000009 *** .03999999999999915
;; 1000000021 *** .03999999999999915
;; end of search

;; 10000000019 *** .129999999999999
;; 10000000033 *** .11999999999999744
;; 10000000061 *** .129999999999999
;; end of search

;; 100000000003 *** .39000000000000057
;; 100000000019 *** .39000000000000057
;; 100000000057 *** .379999999999999
;; end of search

;; 1000000000039 *** 1.25
;; 1000000000061 *** 1.240000000000002
;; 1000000000063 *** 1.2100000000000009
;; end of search

;; The results are slightly larger than we expected.
;; I guess it is because the procedure
;; (next test-divisor) costs more time than
;; (+ test-divisor 1).

;; Exercise 1.29
(define (simpson-sum term a next b k n)
  (define (factor x)
    (cond ((or (= x 0)
               (= x n))
           1)
          ((even? x) 2.0)
          (else 4.0)))
  (if (> k n)
      0
      (+ (* (term a) (factor k))
         (simpson-sum term (next a) next b (+ k 1)
                      n))))

(define (simpson-rule f a b n)
  (define (next x)
    (+ x (/ (- b a) n)))
  (* (/ (/ (- b a) n) 3)
     (simpson-sum f a next b 0 n)))

;; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) 
              (* (term a) result))))
  (iter a 1))

(define (pi n)
  (define (term k)
    (if (even? k)
        (/ (+ k 2)
           (+ k 1))
        (/ (+ k 1)
           (+ k 2))))
  (* 4.0
     (product term 1 add-one n)))

;; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value
                            term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (term a) result))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;; Exercise 1.33
(define (filter-accumulate filter combiner null-value
                           term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a)
                            (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (sum-prime a b)
  (filter-accumulate prime? + 0 square a add-one b))

(define (product-prime n)
  (define (relatively-prime? i)
    (= (gcd i n) 1))
  (filter-accumulate relatively-prime? * 1
                     identity 1 add-one (- n 1)))

;; Exercise 1.34
;; That would be an error.
;; (f f) will be evaluated as (2 2)

;; Exercise 1.35
;; Just call (fixed-point phi 1.0)
(define (phi x)
  (+ 1
     (/ 1 x)))

;; Exercise 1.36
;; The number of steps using average damping is
;; less than that using normal method.

;; Exercise 1.37
(define (cont-frac n d k)
  (define (cont-frac-iter n d count result)
    (if (= count 0)
        result
        (cont-frac-iter n d
                        (- count 1)
                        (/ (n count)
                           (+ result (d count))))))
  (cont-frac-iter n d k 0))

(define (cont-frac n d k)
  (define (cont-frac-recur n d i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i)
              (cont-frac-recur n d (+ i 1))))))
  (cont-frac-recur n d 1))

;; Exercise 1.38
(cont-frac (lambda (i) 1.0)
           (lambda (i)
             (let ((n (remainder i 3)))
               (cond ((= n 0) 1)
                     ((= n 1) 1)
                     ((= n 2) (* (+ (truncate (/ i 3)) 1)
                                 2)))))
           100)

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k))

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a (square x))
       (* b x)
       c)))

;; Exercise 1.41
21

;; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Exercise 1.43
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

;; Exercise 1.44
(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ 
        (f (- x dx))
        (f x)
        (f (+ x dx)))
       3)))

(define (n-fold-smooth f n)
  (lambda (x)
    ((repeated (smooth f) n) x)))

;; Exercise 1.45
(define (nth-roots x n)
  (fixed-point 
   ((repeated average-damp (log 2 n)) 
    (lambda (y)
      (/ x
         (power y (- n 1)))))
   1.0))
;; Notice that there are no built-in function to
;; calculate (log 2 n), I just assume that it is
;; available.

;; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (let ((new-guess (improve guess)))
      (if (good-enough? guess new-guess)
        new-guess
        ((iterative-improve good-enough? improve)
         new-guess)))))

(define (good-enough? v1 v2)
  (< (abs (- v1 v2)) 0.00001))

(define (fixed-point f guess)
  ((iterative-improve good-enough? f) guess))

(define (sqrt x)
  ((iterative-improve good-enough?
                      (lambda (guess)
                        (average guess (/ x guess))))
   1.0))