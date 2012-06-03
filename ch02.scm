;; Exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d))
        (dif-sign? (< (* n d) 0)))
    (if dif-sign?
        (cons (- (abs (/ n g)))
              (abs (/ d g)))
        (cons (abs (/ n g))
              (abs (/ d g))))))

;; Exercise 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (make-point
   (/ (+ (x-point (start-segment seg))
         (x-point (end-segment seg)))
      2)
   (/ (+ (y-point (start-segment seg))
         (y-point (end-segment seg)))
      2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

;; Exercise 2.3
(define (area rect)
  (* (length rect)
     (width rect)))

(define (perimeter rect)
  (* (+ (length rect)
        (width rect))
     2))

;; implementation 1
(define (make-rectangle p length width)
  (cons p (cons length width)))

(define (length rect)
  (car (cdr rect)))

(define (width rect)
  (cdr (cdr rect)))

;; implementation 2
(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (length rect)
  (abs (- (x-point (car rect))
          (x-point (cdr rect)))))

(define (width rect)
  (abs (- (y-point (car rect))
          (y-point (cdr rect)))))

;; Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;; Exercise 2.5
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car n)
  (define (car-iter x count)
    (if (not (= (remainder x 2) 0))
        count
        (car-iter (/ x 2) (+ count 1))))
  (car-iter n 0))

(define (cdr n)
  (define (cdr-iter x count)
    (if (not (= (remainder x 3) 0))
        count
        (cdr-iter (/ x 3) (+ count 1))))
  (cdr-iter n 0))

;; Exercise 2.6
(define one (lambda (f)
              (lambda (x) (f x))))
(define two (lambda (f)
              (lambda (x) (f (f x)))))
(define (+ a b)
  (lambda (f)
    (lambda (x) ((a f) ((b f) x)))))

;; Exercise 2.7
(define (lower-bound r) (car r))
(define (upper-bound r) (cdr r))

;; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

;; Exercise 2.9
(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

;; For any interval x and y, there must be
;; (width (add-interval x y))
;; = (+ (width x) (width y))
;; (width (sub-interval x y))
;; = (+ (width x) (width y))

;; Exercise 2.10
(define (div-interval x y)
  (if (or (= (upper-bound y) 0)
          (= (lower-bound y) 0))
      (error "Cannot divide by zero.")
      (mul-interval (make-interval
                     (/ 1 (upper-bound y))
                     (/ 1 (lower-bound y))))))

;; Exercise 2.11
(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y))
        (pxl (positive? xl))
        (pxu (positive? xu))
        (pyl (positive? yl))
        (pyu (positive? yu)))
    (cond ((and pxl pxu pyl pyu)
           (make-interval (* xl yl) (* xu yu)))
          ((and (not pxl) pxu pyl pyu)
           (make-interval (* xl yu) (* xu yu)))
          ((and (not pxl) (not pxu) pyl pyu)
           (make-interval (* xl yu) (* xu yl)))
          ((and pxl pxu (not pyl) pyu)
           (make-interval (* xu yl) (* xu yu)))
          ((and (not pxl) pxu (not pyl) pyu)
           (make-interval (min (* xl yu)
                               (* xu yl))
                          (max (* xl yl)
                               (* xu yu))))
          ((and (not pxl) (not pxu) (not pyl) pyu)
           (make-interval (* xl yu) (* xl yl)))
          ((and pxl pxu (not pyl) (not pyu))
           (make-interval (* xu yl) (* xl yu)))
          ((and (not pxl) pxu (not pyl) (not pyu))
           (make-interval (* xu yl) (* xl yl)))
          ((and (not pxl) (not pxu) (not pyl) (not pyu))
           (make-interval (* xu yu) (* xl yl))))))

;; Exercise 2.12
(define (make-center-percent center percent)
  (let ((tolerance (* center (/ percent 100))))
    (make-interval (- center tolerance)
                   (+ center tolerance))))

(define (percent x)
  (* (/ (/ (- (upper-bound x) (lower-bound x))
           2)
        center)
     100))

(define (center x)
  (/ (+ (upper-bound x) (lower-bound x))
     2))

;; Exercise 2.13
percentage-tolerance = 
  ((tolerance x) / (center x) +
   (tolerance y) / (center y)) * 100

;; Exercise 2.17
(define (last-pair items)
  (if (= (length items) 1)
      items
      (last-pair (cdr items))))

;; Exercise 2.18
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

;; Exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

;; Exercise 2.20
(define (same-parity x . params)
  (define (same? a b)
    (equal? (even? a) (even? b)))
  (define (same-parity-helper items)
    (cond ((null? items) '())
          ((same? x (car items))
           (cons (car items) 
                 (same-parity-helper (cdr items))))
          (else
           (same-parity-helper (cdr items)))))
  (same-parity-helper params))

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (square x)) items))

;; Exercise 2.23
(define (for-each proc items)
  (if (null? items)
      #t
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

;; Exercise 2.24
(1 (2 (3 4)))

;; Exercise 2.25
(cdr (car (cdr (cdr x))))
(car (car x))
(car (cdr (car (cdr (car (cdr 
     (car (cdr (car (cdr (car (cdr x1))))))))))))

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(1 2 3 4 5 6)

(cons x y)
((1 2 3) 4 5 6)

(list x y)
((1 2 3) (4 5 6))

;; Exercise 2.27
(define (deep-reverse x)
  (cond ((null? x) '())
        ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x))
                      (list (deep-reverse (car x)))))))

;; Exercise 2.28
(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

;; Exercise 2.29
;; a
(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; b
(define (weight? structure)
  (not (pair? structure)))

(define (total-weight mobile)
  (if (weight? mobile)
      mobile
      (+ (total-weight 
          (branch-structure (left-branch mobile)))
         (total-weight
          (branch-structure (right-branch mobile))))))

;; c
(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (if (weight? mobile)
      #t
      (let ((lb (left-branch mobile))
            (rb (right-branch mobile)))
        (and (= (torque lb) (torque rb))
             (balanced? (branch-structure lb))
             (balanced? (branch-structure rb))))))

;; d
(define (make-mobile left right)
  (cons left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (make-branch length structure)
  (cons length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

;; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

;; Exercise 2.32
(define (subsets set)
  (if (null? set)
      (list '())
      (let ((rest (subsets (cdr set))))
        (append rest
                (map (lambda (item)
                       (cons (car set) item))
                     rest)))))

;; Exercise 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0 sequence))

;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* higher-terms x)))
              0
              coefficient-sequence))

;; Exercise 2.35
(define (count-leaves tree)
  (accumulate (lambda (x y) (+ y 1))
              0
              (enumerate-tree tree)))

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons 
       (accumulate op init
                   (accumulate (lambda (x y)
                                 (cons (car x) y))
                               '()
                               seqs))
       (accumulate-n op init
                     (accumulate (lambda (x y)
                                   (cons (cdr x) y))
                                 '()
                                 seqs)))))

;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product v w))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (accumulate (lambda (x y)
                         (cons (dot-product v x)
                               y))
                         '() cols))
         m)))

;; Exercise 2.38
(fold-right / 1 (list 1 2 3))
;; 3/2
(fold-left / 1 (list 1 2 3))
;; 1/6
(fold-right list '() (list 1 2 3))
;; (1 (2 (3 ())))
(fold-left list '() (list 1 2 3))
;; (((() 1) 2) 3)

;; When op is '+' or '*', fold-left and fold-right will
;; produce the same result.

;; Exercise 2.39
(define (reverse seq)
  (fold-right (lambda (x y)
                (append y (list x)))
              '() seq))

(define (reverse seq)
  (fold-left (lambda (x y)
               (cons y x))
             '() seq))

;; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 i)))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; Exercise 2.41
(define (sum-triple n s)
  (filter (lambda (x)
            (= (+ (car x) (cadr x) (caddr x)) s))
          (flatmap 
           (lambda (i)
             (flatmap 
              (lambda (j)
                (map (lambda (k)
                       (list i j k))
                     (enumerate-interval 1 (- j 1))))
              (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n))))

;; Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (row p) (car p))
(define (col p) (cadr p))
(define (make-pos x y) (list x y))

(define empty-board '())

(define (safe? k positions)
  (define (safe-pair? p1 p2)
    (and (not (= (row p1) (row p2)))
         (not (= (abs (- (row p1) (row p2)))
                 (abs (- (col p1) (col p2)))))))
  (define (remove set x)
    (filter (lambda (y)
              (not (= x (col y))))
            set))
  (define (safe-iter new rest)
    (if (null? rest)
        #t
        (and (safe-pair? new (car rest))
             (safe-iter new (cdr rest)))))
  (safe-iter (list-ref positions (- k 1))
             (remove positions k)))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (make-pos new-row k))))

;; Exercise 2.43
;; In Louis's method, all the possible positions are 
;; generated before they are filtered. So it will take
;; quite a long time to calculate the results.

;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; Exercise 2.45
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 smaller smaller))))))

;; Exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor v1) (xcor v2))
             (+ (ycor v1) (ycor v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor v1) (xcor v2))
             (- (ycor v1) (ycor v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor v))
             (* s (ycor v))))

;; Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

;; Another implementation.
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (car (cdr f)))

(define (edge2-frame f)
  (cdr (cdr f)))

;; Exercise 2.48
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;; Exercise 2.49
(define painter-a
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 1) (make-vect 1 0))
    (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define painter-b
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define painter-c
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
    (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
    (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
    (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))))

;; Exercise 2.50
(define (flip-horiz painter)
  (lambda (frame)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))))

(define (rotate180 painter)
  (lambda (frame)
    (transform-painter painter
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0))))

(define (rotate270 painter)
  (lambda (frame)
    (transform-painter painter
                       (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))))

;; Exercise 2.51
(define (below top bottom)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top (transform-painter
                      top
                      split-point
                      (make-vect 1.0 0.5)
                      (make-vect 0.0 1.0)))
          (paint-bottom (transform-painter
                         bottom
                         (make-vect 0.0 0.0)
                         (make-vect 1.0 0.0)
                         split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below top bottom)
  (lambda (frame)
    (rotate90 (beside (rotate270 top) (rotate270 bottom)))))

;; Exercise 2.53
(list 'a 'b 'c)
;; (a b c)

(list (list 'george))
;; ((george))

(cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; (y1 y2)

(pair? (car '(a short list)))
;; #f

(memq 'red '((red shoes) (blue socks)))
;; #f

(memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

;; Exercise 2.54
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (eq? a b)))

;; Exercise 2.55
;; (car ''abracadabra) can be rewritten as 
;; (car '(quote abracadabra)), the first of element of
;; list (quote abracadabra) is quote.

;; Exercise 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponentiation
                         (base exp)
                         (- (exponent exp) 1))
                        (deriv (base exp) var))))
        (else (error "Invalid expression."))))

(define (make-exponentiation b e)
  (cond ((=number? e 0)
         (if (not (=number? b 0))
             1
             (error "exponent error")))
        ((=number? e 1) b)
        (else (list '** b e))))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

;; Exercise 2.57
(define (augend s)
  (let ((x (cddr s)))
    (if (pair? (cdr x))
        (cons '+ x)
        (car x))))

(define (multiplicand p)
  (let ((x (cddr p)))
    (if (pair? (cdr x))
        (cons '* x)
        (car x))))

;; Exercise 2.58
;; a
;; Change constructors and selectors as below.
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(define (sum? s)
  (and (pair? s) (eq? (cadr s) '+)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

(define (product? p)
  (and (pair? p) (eq? (cadr p) '*)))

;; Exercise 2.58
;; Again we only need to change the predicates and
;; selectors.
(define (op-exists? op s)
  (memq op s))

(define (sum? exp)
  (op-exists? '+ exp))

(define (product? exp)
  (and (op-exists? '* exp)
       (not (op-exists? '+ exp))))

(define (addend exp)
  (define (addend-helper x)
    (if (same-variable? (car x) '+)
        '()
        (cons (car x) (addend-helper (cdr x)))))
  (let ((a (addend-helper exp)))
    (if (= (length a) 1)
        (car a)
        a)))

(define (augend exp)
  (let ((a (cdr (memq '+ exp))))
    (if (= (length a) 1)
        (car a)
        a)))

;; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) 
                    (union-set (cdr set1) set2)))))

;; Exercise 2.60
;; O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; O(1)
(define (adjoin-set x set)
  (cons (x set)))

;; O(n)
(define (union-set set1 set2)
  (append set1 set2))

;; O(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set)
                               (adjoin-set x (cdr set))))))

;; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (union-set (cdr set1)
                                   (cdr set2))))
                 ((< x1 x2)
                  (cons x1
                        (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2
                        (union-set set1 (cdr set2)))))))))

;; Exercise 2.63
;; The two procedures produce the same results and 
;; have the same order of growth.

;; Exercise 2.65
(define (union-set set1 set2)
  (define (union-seq seq1 seq2)
    (cond ((null? seq1) seq2)
          ((null? seq2) seq1)
          ((= (car seq1) (car seq2))
           (cons (car seq1)
                 (union-set (cdr seq1) (cdr seq2))))
          ((< (car seq1) (car seq2))
           (cons (car seq1)
                 (union-set (cdr seq1) seq2)))
          ((> (car seq1) (car seq2))
           (cons (car seq2)
                 (union-set seq1 (cdr seq2))))))
  (list->tree (union-seq (tree->list set1)
                         (tree->list set2))))

(define (intersection-set set1 set2)
  (define (intersection-seq seq1 seq2)
    (cond ((or (null? seq1) (null? seq2)) '())
          ((= (car seq1) (car seq2))
           (cons (car seq1)
                 (intersection-seq (cdr seq1)
                                   (cdr seq2))))
          ((< (car seq1) (car seq2))
           (intersection-seq (cdr seq1) seq2))
          ((> (car seq1) (car seq2))
           (intersection-seq seq1 (cdr seq2)))))
  (list->tree (intersection-seq (tree->list set1)
                                (tree->list set2))))

;; Exercise 2.66
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((record (entry set-of-records)))
        (cond ((= given-key (key record)) record)
              ((< given-key (key record))
               (lookup given-key 
                       (left-branch set-of-records)))
              ((> given-key (key record))
               (lookup given-key 
                       (right-branch set-of-records)))))))

;; Exercise 2.67
;; (a d a b b c a)

;; Exercise 2.68
(define (encode-symbol symbol tree)
  (define (exists-symbol symbol branch)
    (memq symbol (symbols branch)))
  (cond ((leaf? tree) '())
        ((exists-symbol symbol (left-branch tree))
         (cons '0 (encode-symbol symbol (left-branch tree))))
        ((exists-symbol symbol (right-branch tree))
         (cons '1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol error"))))

;; Exercise 2.69
(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      leaf-set
      (successive-merge
       (adjoin-set (make-code-tree (car leaf-set)
                                   (cadr leaf-set))
                   (cddr leaf-set)))))

;; Exercise 2.70
;; We need 84 bits to encode the song. And if we use 
;; a fixed length method, each symbol will need 3 bits
;; to represent itself. So the total number of bits 
;; will be 108.

;; Exercise 2.71
;; most frequent symbol : 0
;; least frequent symbol : n - 1

;; Exercise 2.72
;; most frequent symbol : O(1)
;; least frequent symbol : O(n^2)

;; Exercise 2.73
;; a
;; Because they are not generic operations, and need not be dispatched.

;; b
(define (make-sum x y)
  (cons '+ (cons x y)))

(define (install-deriv-sum-package)
  (define (addend exp) (car exp))
  (define (augend exp) (cdr exp))

  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'deriv '+ sum-deriv)
  'done)

(define (make-product x y)
  (cons '* (cons x y)))

(define (install-product-package)
  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (cdr exp))

  (define (product-deriv exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))

  (put 'deriv '* product-deriv)
  'done)

;; c
(define (make-exponentiation b e)
  (cons '** (cons b e)))

(define (install-exponentiation-package)
  (define (base exp) (car exp))
  (define (exponent exp) (cdr exp))

  (define (exponentiation-deriv exp var)
    (make-product (exponent exp)
                  (make-product
                   (make-exponentiation
                    (base exp)
                    (- (exponent exp) 1))
                   (deriv (base exp) var))))
  
  (put 'deriv '** exponentiation-deriv)
  'done)

;; d
;; We need to change put operations as below.
;; (put <type> <op> <item>)

;; Exercise 2.74
;; a
(define (get-record name file)
  (let ((proc (get 'retrieve-record (type-file file))))
    (proc name (content-file file))))

;; b
(define (get-salary record)
  (let ((proc (get 'salary (type-record record))))
    (proc (content-record record))))

;; c
(define (find-employee-record name files)
  (if (null? files)
      #f
      (let ((proc (get 'retrieve-record (type-file (car files)))))
        (let ((result (proc name (car files))))
          (if result
              result
              (find-employee-record name (cdr files)))))))
              
;; d
;; In order to incorporate into the central system,
;; we need to add type info for files and records,
;; and implement interfaces to retrieve record and
;; members of record. Moreover, we need to put 
;; procedures into the data directed table so that 
;; the new personnel information can be found by 'get'.

;; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else
           (error "Unknown op."))))
  dispatch)

;; Exercise 2.76
;; data-directed-style is most appropriate for a system
;; in which new types must often be added.
;; message-passing-style is most appropriate for a system
;; in which new operations must often be added.

;; Exercise 2.77
;; apply-generic is invoked twice.
(magnitude z) =>
(apply-generic 'magnitude z) =>
;; z is tagged as complex, so we get magnitude again.
(magnitude z) =>
(apply-generic 'magnitude z)
;; z is tagged as rectangle or polar, we get the 
;; real magnitude procedure.

;; Exercise 2.78
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged data"))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged data"))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

;; Exercise 2.78
;; generic arithmetic package
(define (equ? x y)
  (apply-generic 'equ? x y))

;; scheme-number package
(put 'equ? '(scheme-number scheme-number) =)

;; rational package
(define (equ?-rational x y)
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))
(put 'equ? '(rational rational) equ?-rational)

;; complex package
(define (equ?-complex x y)
  (and (= (real-part x) (real-part y))
       (= (imag-part x) (imag-part y))))
(put 'equ? '(complex) equ?-complex)

;; Exercise 2.79
;; generic arithmetic package
(define (=zero? x)
  (apply-generic '=zero? x))

;; scheme-number package
(put '=zero? '(scheme-number)
     (lambda (x) (= x 0)))

;; rational package
(define (=zero?-rational x)
  (and (= (numer x) 0)
       (= (denom x) 0)))
(put '=zero? '(rational) =zero?-rational)

;; complex package
(define (=zero?-complex x y)
  (and (= (real-part x) 0)
       (= (imag-part x) 0)))
(put '=zero? '(complex) =zero?-complex)