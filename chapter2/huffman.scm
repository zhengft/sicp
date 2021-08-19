(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      ()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((eq? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (encode-symbol x tree)
  (define (encode-symbol-1 x current-branch)
    (cond ((leaf? current-branch) ())
	  ((element-of-set? x (symbols (left-branch current-branch)))
	   (cons 0 (encode-symbol-1 x (left-branch current-branch))))
	  (else (cons 1 (encode-symbol-1 x (right-branch current-branch))))))
  (if (element-of-set? x (symbols tree))
      (encode-symbol-1 x tree)
      (error "bad symbol -- ENCODE-SYMBOL" x)))

(define (encode message tree)
  (if (null? message)
      ()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge code-tree-set)
  (if (null? (cdr code-tree-set))
      (car code-tree-set)
      (successive-merge (adjoin-set (make-code-tree (car code-tree-set)
						    (cadr code-tree-set))
				    (cddr code-tree-set)))))

(define sample-pairs
  '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
