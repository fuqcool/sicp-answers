;; Exercise 3.1
(define (make-accumulator sum)
  (lambda (x)
    (set! sum (+ x sum))
    sum))

;; Exercise 3.2
(define (make-monitored f)
  (define count 0)
  (lambda (x)
    (if (eq? x 'how-many-calls?)
        count
        (begin (set! count (+ count 1))
               (f x)))))

;; Exercise 3.3, 3.4
(define (make-account password balance)
  (define wrong-times 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (begin
          (set! wrong-times 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error
                       "Unknown request -- MAKE-ACCOUNT"
                       m))))
        (begin
          (set! wrong-times (+ wrong-times 1))
          (if (< wrong-times 7)
              (error "Wrong password -- " 
                       wrong-times " times")
              (error "Run, run, cops are coming!")))))
  dispatch)


;; Exercise 3.5
(define (random-in-range low high)
   (let ((range (- high low)))
    (+ low (random range))))
(define (contain-test)
  (<= (+ (square (- (random-in-range 0.0 2.0) 1))
         (square (- (random-in-range 0.0 2.0) 1)))
      1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) 
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
(define (estimate-integral trials)
  (* 4.0 (monte-carlo trials contain-test)))

;; Exercise 3.6
(define rand
  (let ((x random-init))
    (lambda (action)
      (cond ((eq? action 'generate)
             (begin
               (set! x (rand-update x))
               x))
            ((eq? action 'reset)
             (lambda (new-value)
               (set! x new-value)))
            (else
             (error "Unknown request"))))))

;; Exercise 3.7
(define (make-joint acc password new-password)
  ((acc password 'add-password) new-password)
  acc)

(define (make-account password balance)
  (define wrong-times 0)
  (define password-list (list password))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (valid-password p)
    (memq p password-list))
  (define (add-password p)
    (set! password-list (cons p password-list)))
  (define (dispatch p m)
    (if (valid-password p)
        (begin
          (set! wrong-times 0)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 ((eq? m 'add-password) add-password)
                 (else (error
                        "Unknown request -- MAKE-ACCOUNT"
                        m))))
        (begin
          (set! wrong-times (+ wrong-times 1))
          (if (< wrong-times 7)
              (error "Wrong password -- "
                       wrong-times " times")
              (error "Run, run, cops are coming!")))))
  dispatch)

;; Exercise 3.8
(define f
  (let ((a 0))
    (lambda (x)
      (cond ((= x 0)
             (begin
               (set! a (+ a 1))
               a))
            ((= x 1)
             (begin
               (set! a (- a))
               a))))))

;; Exercise 3.12
;; (b)
;; (b c d)

;; Exercise 3.13
;; Compute (last-pair z) will cause an infinite loop.

;; Exercise 3.14
;; w => (d c b a)
;; v => (a b c d)

;; Exercise 3.17
(define (count-pairs x)
  (define counted-pairs '())
  (define (count-pairs-iter s)
    (define (counted? pair counted-pairs)
      (cond ((null? counted-pairs) #f)
            ((eq? pair (car counted-pairs)) #t)
            (else (counted? pair (cdr counted-pairs)))))

    (define (count pair)
      (if (counted? pair counted-pairs) 
          0 
          (begin 
            (set! counted-pairs (cons pair counted-pairs))
            1)))

    (if (not (pair? s))
        0
        (+ (count s)
           (count-pairs-iter (car s))
           (count-pairs-iter (cdr s)))))
  (count-pairs-iter x))

;; Exercise 3.18
(define (infinite-list? x)
  (define tracking-list '())

  (define (exists-pair? x set)
    (cond ((null? set) #f)
          ((eq? x (car set)) #t)
          (else (exists-pair? x (cdr set)))))

  (define (infinite-list?-iter x)
    (if (null? x)
        #f
        (if (exists-pair? x tracking-list)
            #t
            (begin
              (set! tracking-list (cons x tracking-list))
              (infinite-list?-iter (cdr x))))))
  (infinite-list?-iter x))

;; Exercise 3.21
(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "queue is empty")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "queue is empty")
      (begin
        (set-front-ptr! queue (cdr (front-ptr queue)))
        queue)))

(define (print-queue queue)
  (display (front-ptr queue)))

;; Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (if (empty-queue?)
            (begin
              (set! front-ptr new-pair)
              (set! rear-ptr new-pair)
              front-ptr)
            (begin
              (set-cdr! rear-ptr new-pair)
              (set! rear-ptr new-pair)
              front-ptr))))

    (define (delete-queue!)
      (set! front-ptr (cdr front-ptr))
      front-ptr)

    (define (print-queue)
      (display front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (car front-ptr))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'print-queue) (print-queue))
            (else (error "no such method"))))
    dispatch))

;; Exercise 2.23
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (make-node item) (list item '() '()))
    (define (node-value node) (car node))
    (define (next-node node) (caddr node))
    (define (prev-node node) (cadr node))
    (define (set-node-prev! node item)
      (set-car! (cdr node) item))
    (define (set-node-next! node item)
      (set-car! (cddr node) item))

    (define (front-deque) 
      (if (empty-deque?)
          (error "deque is empty.")
          (node-value front-ptr)))
    (define (empty-deque?)
      (null? front-ptr))
    (define (print-deque)
      (define (print-deque-helper node)
        (if (not (null? node))
            (begin
              (display (node-value node))
              (display " ")
              (print-deque-helper (next-node node)))))
      (if (empty-deque?)
          (display "()")
          (begin
            (display "(")
            (print-deque-helper front-ptr)
            (display ")"))))
            
    (define (front-insert-deque! item)
      (let ((new-node (make-node item)))
        (if (empty-deque?)
            (begin
              (set! front-ptr new-node)
              (set! rear-ptr new-node)
              (print-deque))
            (begin
              (set-node-next! new-node front-ptr)
              (set-node-prev! front-ptr new-node)
              (set! front-ptr new-node)
              (print-deque)))))

    (define (front-delete-deque!)
      (if (empty-deque?)
          (error "deque is empty")
          (begin
            (set! front-ptr (next-node front-ptr))
            (print-deque))))

    (define (rear-insert-deque! item)
      (let ((new-node (make-node item)))
        (if (empty-deque?)
            (begin
              (set! front-ptr new-node)
              (set! rear-ptr new-node)
              (print-deque))
            (begin
              (set-node-next! rear-ptr new-node)
              (set-node-prev! new-node rear-ptr)
              (set! rear-ptr new-node)
              (print-deque)))))

    (define (rear-delete-deque!)
      (if (empty-deque?)
          (error "deque is empty.")
          (begin
            (set! rear-ptr (prev-node rear-ptr))
            (set-node-next! rear-ptr '())
            (print-deque))))

    (define (dispatch m)
      (cond ((eq? m 'front-deque) (front-deque))
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'front-delete-deque!) (front-delete-deque!))
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
            ((eq? m 'empty-deque?) (empty-deque?))
            ((eq? m 'print-deque) (print-deque))
            (else (error "no such method"))))
    dispatch))

;; Exercise 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation"))))
    dispatch))

;; Exercise 3.25
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (is-table? obj) (list? obj))
    (define (is-record? obj) (and obj (not (list? obj))))
    (define (assoc keys records)
      (let ((key (car keys)))
        (if (null? records)
            #f
            (let ((record (car records)))
              (if (and (equal? key (car record))
                       (if (null? (cdr keys)) (is-record? record) (is-table? record)))
                  record
                  (assoc keys (cdr records)))))))

    (define (lookup keys table)
      (display keys)
      (display table)
      (let ((key (car keys)))
        (let ((record (assoc keys (cdr table))))
          (cond ((is-record? record) (cdr record))
                ((is-table? record) (lookup (cdr keys) record))
                (else #f)))))

    (define (make-subtable keys value)
      (let ((key (car keys)))
        (if (null? (cdr keys))
            (cons key value)
            (list key (make-subtable (cdr keys) value)))))

    (define (insert! keys value table)
      (let ((key (car keys)))
        (let ((record (assoc keys (cdr table))))
          (cond ((is-record? record) (set-cdr! record value))
                ((is-table? record) (insert! (cdr keys) value record))
                (else
                 (set-cdr! table
                           (cons (make-subtable keys value)
                                 (cdr table))))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup) (lambda (keys)
                                    (lookup keys local-table)))
            ((eq? m 'insert!) (lambda (keys value)
                                     (insert! keys value local-table)
                                     'ok))
            ((eq? m 'print) (display local-table))
            (else (error "Unknown operation"))))
    dispatch))

;; Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let (new-value
          (logical-or (get-signal a1) (get-signal a2)))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; Exercise 3.29
;; delay time = 2 * inverter-delay + and-gate-delay
(define (or-gate a1 a2 output)
  (let ((b1 (make-wire))
        (b2 (make-wire))
        (c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)))

;; Exercise 3.30
(define (ripple-carry-adder a b s c)
  (if (null? a)
      'ok
      (let ((c-out (make-wire)))
        (full-adder (car a) (car b) c (car s) c-out)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-out))))

;; Exercise 3.31
;; If we don't run action as soon as we add it, the
;; signal won't change until the we give it a new value,
;; and before that, the circut won't work correctly.

