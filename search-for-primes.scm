(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m)) m))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))


(define (check-result base result m)
  (if (and (not (= base 1))
           (not (= base (- m 1)))
           (= result 1))
      0
      result))

(define (expmod-and-check base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (check-result base (remainder (square (expmod base (/ exp 2) m)) m) m))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (mr-test n)
  (define (try-it a)
    (= (expmod-and-check a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-search-for-primes n start-time)
  (newline)
  (display n)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (start-search-for-primes (+ n 1) (runtime))))

(define (search-for-primes n)
  (start-search-for-primes n (runtime)))

(define (start-search-for-fast-primes n start-time)
  (newline)
  (display n)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      (start-search-for-primes (+ n 1) (runtime))))

(define (search-for-fast-primes n)
  (start-search-for-fast-primes n (runtime)))

