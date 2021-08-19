(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
	count
	(length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (last-pair items)
  (define (last-pair-iter a l)
    (if (null? a)
	l
	(last-pair-iter (cdr a) (car a))))
  (last-pair-iter items items))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (append-item list1 item)
  (if (null? list1)
      (list item)
      (cons (car list1) (append-item (cdr list1) item))))

(define (reverse items)
  (if (null? (cdr items))
      items
      (append-item (reverse (cdr items)) (car items))))

(define (deep-reverse items)
  (cond ((not (pair? items)) items)
	((null? (cdr items)) (list (deep-reverse (car items))))
	(else (append-item (deep-reverse (cdr items)) (deep-reverse (car items))))))
