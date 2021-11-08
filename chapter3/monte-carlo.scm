(define (rand)
  (define random-init 0)
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
    x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (circle-test x y)
  (<= (+ (* (- x 5) (- x 5)) (* (- y 7) (- y 7))) 9))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (integral-test)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* (- x2 x1) (- y2 y1)) (monte-carlo trials integral-test)))

(define rand
  (let ((randx 0))
    (define (rand-update x)
      (+ x 1))
    (define (generate)
      (set! randx (rand-update randx))
      randx)
    (define (reset x)
      (set! randx x)
      randx)
    (lambda (x)
      (cond ((eq? x 'generate) (generate))
            ((eq? x 'reset) reset)
            (else (error "Unknown request -- RAND" x))))))