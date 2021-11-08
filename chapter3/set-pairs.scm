(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y))

; (define (last-pair x)
;   (if (null? x)
;       x
;       (last-pair (cdr x))))
; 
; (define (append! x y)
;   (set-car! (last-pair x) y))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
      (+ (count-pairs (cdr x))
      1))))

(define c (cons 'c '()))
(define b (cons 'b c))
(define a (cons b c))

(define c (cons 'c '()))
(define b (cons c c))
(define a (cons b b))

(define loop-list '(a b c))
(make-cycle loop-list)

(define (does-visited p v)
  (cond ((null? v) #f)
        ((eq? (car v) p) #t)
        (else (does-visited p (cdr v)))))

(define (count-pairs x)
  (define (iter current visited need-visit result)
    (cond ((pair? current)
           (if (does-visited current visited)
               (if (null? need-visit)
                   result
                   (iter (car need-visit) visited (cdr need-visit) result))
               (iter (car current) (cons current visited) (cons (cdr current) need-visit) (+ result 1))))
          (else
           (if (null? need-visit)
               result
               (iter (car need-visit) visited (cdr need-visit) result)))))
  (iter x () () 0))

(define (check-loop x)
  (define (iter current visited)
    (if (pair? current)
      (if (does-visited current visited)
        #t
        (iter (cdr current) (cons current visited)))
      #f))
  (iter x ())
)

(define (contains-cycle? lst) 
  (define (safe-cdr l) 
    (if (pair? l) 
        (cdr l) 
        '())) 
  (define (iter a b) 
    (cond ((not (pair? a)) #f) 
          ((not (pair? b)) #f) 
          ((eq? a b) #t) 
          ((eq? a (safe-cdr b)) #t) 
          (else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))))) 
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))