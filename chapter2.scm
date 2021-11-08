(load "gcd.scm")

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((nn (/ n g))
          (dd (/ d g)))
      (if (< dd 0)
        (cons (- nn) (- dd))
        (cons nn dd)))))

; (define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (scale-list items factor)
  (if (null? items)
      ()
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (for-each proc items)
  (define (do-for-each)
    (proc (car items))
    (for-each proc (cdr items)))
    
  (if (null? items)
      #t
      (do-for-each)))

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define (fringe x)
  (cond ((null? x) ())
        ((not (pair? x)) (list x))
	(else (append (fringe (car x)) (fringe (cdr x))))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch x) (car x))

(define (right-branch x) (car (cdr x)))

(define (branch-length x) (car x))

(define (branch-structure x) (car (cdr x)))

(define (total-branch-weight x)
  (cond ((not (pair? (right-branch x))) (branch-structure x))
	(else (total-weight (branch-structure x)))))

(define (total-weight x)
  (+ (total-branch-weight (left-branch x)) (total-branch-weight (right-branch x))))

(define (branch-torques x)
  (* (branch-length x) (total-weight (branch-structure x))))

(define (branch-balance? x)
  (cond ((not (pair? (right-branch x))) #t)
        (else (balance? (branch-structure x)))))

(define (balance? x)
  (and (branch-balance? (left-branch x))
       (branch-balance? (right-branch x))
       (= (branch-torques (left-branch x)) (branch-torques (right-branch x)))))

(define (scale-tree tree factor)
  (cond ((null? tree) ())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))))

(define (square-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) ())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (accumulate
   (lambda (x y)
     (cond ((not (pair? x)) (+ 1 y))
	   (else (+ (count-leaves x) y))))
   0
   t))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda x (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda x (matrix-*-vector cols x)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(map (lambda (k) (list i j k))
			     (enumerate-interval 1 (- j 1))))
		      (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))
     
(define (equal-sum-triples n s)
  (filter (lambda (x) (= s (fold-right + 0 x)))
          (unique-triples n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))


(define (make-triples-sum x)
  (append x (list (fold-right + 0 x))))


(define (sum-triples n s)
  (map make-triples-sum
       (filter (lambda (x) (= (+ (car x) (cadr x) (caddr x)) s))
	       (unique-triples n))))

(define (adjoin-position n k r)
  (cons (list n k) r))

(define (safe? k positions)
  
  (define (safe-left-cross? current others)
    (fold-right (lambda (a b) (and a b))
                #t
                (map (lambda (x)
                       (not (= (+ (car current) (cadr current))
                               (+ (car x) (cadr x)))))
                     others)))

  (define (safe-right-cross? current others)
    (fold-right (lambda (a b) (and a b))
                #t
                (map (lambda (x)
                       (not (= (- (car current) (cadr current))
                               (- (car x) (cadr x)))))
                     others)))

  (define (safe-row? current others)
    (fold-right (lambda (a b) (and a b))
                #t
                (map (lambda (x) (not (= (car current) (car x)))) others)))

  (let ((current (car positions))
        (others (cdr positions)))

       (and (safe-row? current others)
            (safe-right-cross? current others)
            (safe-left-cross? current others)
       )))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list ())
	  (filter
	    (lambda (positions) (safe? k positions))
	    (flatmap
	      (lambda (rest-of-queens)
	        (map (lambda (new-row)
	               (adjoin-position new-row k rest-of-queens))
	             (enumerate-interval 1 board-size)))
	      (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x) ) x)
	(else (memq item (cdr x)))))


(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
	((or (null? a) (null? b)) #f)
	((and (pair? (car a)) (pair? (car b))) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
	((or (pair? (car a)) (pair? (car b))) #f)
	(else (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))))

(define (pair-equal? a b)
  (cond ((and (null? a) (null? b)) #t)
	((or (null? a) (null? b)) #f)
	(else (and (equal? (car a) (car b))
		   (pair-equal? (cdr a) (cdr b))))))

(define (equal? a b)
  (cond ((and (pair? a) (pair? b)) (pair-equal? a b))
	((or (pair? a) (pair? b)) #f)
	(else (eq? a b))))
