(define the-empty-stream '())

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; (define (force delayed-object)
;   (delayed-object))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

; (define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define factorials (cons-stream 1 (mul-streams integers factorials)))

(define (partial-sums s) 
  (define ps (add-streams s (cons-stream 0 ps))) 
  ps)

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                   (cons-stream s1car
                                (merge (stream-cdr s1)
                                       (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define (integrate-series s)
  (stream-map / s integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; ??
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

;; ??
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                              (scale-stream (stream-cdr s2) (stream-car s1)))
                 (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

;;
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) t))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
                (stream-cdr u))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) (car x) (cadr x)))
                 (pairs (stream-cdr t) (stream-cdr u)))
     (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define (triples s t u)
        (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
                     (interleave
                      (stream-map (lambda (x) (cons (stream-car s) x))
                                  (stream-cdr (pairs t u)))
                      (triples (stream-cdr s)
                               (stream-cdr t)
                               (stream-cdr u)))))

(define (phythagorean-numbers)
  (define (match x)
    (= (+ (square (car x) (square (cadr x)))) (square (caddr x))))
  (stream-filter match
                 (triples integers integers integers)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (let ((s1weight (weight s1car))
                  (s2weight (weight s2car)))
              (cond ((<= s1weight s2weight)
                     (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                    (else
                     (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define sum-i-j (weighted-pairs integers
                                integers
                                (lambda (x) (+ (car x) (cadr x)))))

; (stream-filter (lambda (x)
;                  (let ((i (car x))
;                        (j (cadr x)))
;                    (or (divisible? i 2)
;                        (divisible? i 3)
;                        (divisible? i 5)
;                        (divisible? j 2)
;                        (divisible? j 3)
;                        (divisible? j 5))))
;                (weighted-pairs integers
;                                integers
;                                (lambda (x) (+ (* (car x) 2) (* (cadr x) 3) (* (car x) (cadr x) 5)))))

(define (ramanujan-number)
  (define (cube-sum x)
    (let ((i (car x))
          (j (cadr x)))
         (+ (* i i i) (* j j j))))

  (define (generate pairs)
    (let ((a (stream-ref pairs 0))
          (b (stream-ref pairs 1)))
      (let ((sum-a (cube-sum a))
            (sum-b (cube-sum b)))
        (if (= sum-a sum-b)
            (cons-stream (list sum-a a b) (generate (stream-cdr (stream-cdr pairs))))
            (generate (stream-cdr pairs))))))
  (generate (weighted-pairs integers integers cube-sum)))

(define (square-number)
  (define (square-sum x)
    (let ((i (car x))
          (j (cadr x)))
         (+ (* i i) (* j j))))

  (define (generate pairs)
    (let ((a (stream-ref pairs 0))
          (b (stream-ref pairs 1))
          (c (stream-ref pairs 2)))
      (let ((sum-a (square-sum a))
            (sum-b (square-sum b))
            (sum-c (square-sum c)))
        (if (= sum-a sum-b sum-c)
            (cons-stream (list sum-a a b c) (generate (stream-cdr (stream-cdr (stream-cdr pairs)))))
            (generate (stream-cdr pairs)))))
  (generate (weighted-pairs integers integers square-sum))))

(define (RC R C dt)
  (define (proc i v0)
    (cons-stream v0
                 (proc i (+ v0 (/ (* i dt) C) (* R i))))))

(define (RC r c dt)
  (define (proc i v)
    (add-streams (scale-stream i r)
                 (integral (scale-stream i (/ 1 c)) v dt)))
  proc)

; (define zero-crossings
;   (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

(define (smooth s) 
  (stream-map (lambda (x1 x2) (/ (+ x1 x2) 2))
              (cons-stream 0 s)
              s))

(define (make-zero-crossings input-stream)
  (let ((after-smooth (smooth input-stream)))
    (stream-map sign-change-detector after-smooth (cons-stream 0 after-smooth))))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))


(define (random-update x)
         (remainder (+ (* 13 x) 5) 24))
(define random-init (random-update (expt 2 32)))

(define random-numbers
  (cons-stream random-init
               (stream-map random-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p)
                (if (= p 0) 0 (sqrt (/ 6 p))))
              (monte-carlo cesaro-stream 0 0)))
  
;; assume the operation 'generator and 'reset is a stream,
;; and if the command is 'generator, the element of
;; stream is a string, if the command is 'reset,
;; it is a pair whose first element is 'reset,
;; the other element is the reset value.
(define (random-number-generator command-stream) 
  (define random-number 
    (cons-stream random-init 
                 (stream-map (lambda (number command)
                                     (cond ((null? command) the-empty-stream)
                                           ((eq? command 'generator)
                                            (random-update number))
                                           ((and (pair? command)
                                                 (eq? (car command) 'reset))
                                            (cdr command))
                                           (else
                                            (error "bad command -- " commmand))))
                             random-number
                             command-stream)))
  random-number)
