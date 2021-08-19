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
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product (make-exponentiation (base exp)
							  (make-sum (exponent exp) -1))
				     (deriv (base exp) var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b1 e2)
  (cond ((=number? e2 0) 1)
	((=number? e2 1) b1)
	(else (list '** b1 e2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (addend s) (car s))

(define (augend s) (cadr s))

(define (multiplier p) (car p))

(define (multiplicand p) (cadr p))

(define (base e) (car e))

(define (exponent e) (cadr e))
	       
(define (install-sum-package)
  (define (deriv-part rands var)
    (make-sum (deriv (addend rands) var)
	      (deriv (augend rands) var)))
  (put 'deriv '+ deriv-part)

(define (install-product-package)
  (define (deriv-part rands var)
    (make-sum
     (make-product (multiplier rands)
		   (deriv (multiplicand rands) var))
     (make-product (deriv (multiplier rands) var)
		   (multiplicand rands))))
  (put 'deriv '* deriv-part))

(define (install-exponentiation-package)
  (define (deriv-part rands var)
    (make-product (exponent rands)
		  (make-product (make-exponentiation (base rands)
						     (make-sum (exponent rands) -1))
				(deriv (base rands) var))))
  (put 'deriv '** deriv-part))
