(define (same-parity x . z)
  (define (filter-parity f items)
    (if (null? items)
	items
        (let ((first (car items))
	      (rest (filter-parity f (cdr items))))
          (if (f first)
	      (cons first rest)
	      rest))))
  (let ((full (cons x z)))
    (if (odd? x)
        (filter-parity odd? full)
        (filter-parity even? full))))
