(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; (define (mul-interval x y)
;   (let ((p1 (* (lower-bound x) (lower-bound y)))
;         (p2 (* (lower-bound x) (upper-bound y)))
;         (p3 (* (upper-bound x) (lower-bound y)))
;         (p4 (* (upper-bound x) (upper-bound y))))
;     (make-interval (min p1 p2 p3 p4)
;                    (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (cond ((and (>= (lower-bound x) 0) (>= (lower-bound y) 0))
          (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (< (upper-bound x) 0) (< (upper-bound y) 0))
          (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (>= (lower-bound x) 0) (< (upper-bound y) 0))
          (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
        ((and (< (upper-bound x) 0) (>= (lower-bound y) 0))
          (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
        ((and (< (lower-bound x) 0) (>= (upper-bound x)) (>= (lower-bound y) 0))
          (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (< (lower-bound x) 0) (>= (upper-bound x)) (< (upper-bound y) 0))
          (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (>= (lower-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y)))
          (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (< (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y)))
          (make-interval (* (lower-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (< (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y)))
          (let ((p1 (* (lower-bound x) (upper-bound y)))
                (p2 (* (upper-bound x) (lower-bound y)))
                (p3 (* (lower-bound x) (lower-bound y)))
                (p4 (* (upper-bound x) (upper-bound y))))
            (make-interval (min p1 p2) (max p3 p4))))
        (else (error "No such situation"))))


(define (div-interval x y)
  (if (= 0 (lower-bound y)) (error "lower bound is zero")
    (if (= 0 (upper-bound y)) (error "upper bound is zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y)))))))

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (1 (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (percent i)
  (/ (width i) (center i)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))


