(define (assoc key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records) same-key?))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table) same-key?)))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table) same-key?)))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (make-double-key-table same-key?)
  (let ((local-table (list '*table*)))
  (define (lookup key-1 key2)
    (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
      (if subtable
          (let ((record (assoc key-2 (cdr subtable) same-key?)))
            (if record
                (cdr record)
                false))
          false)))
  (define (insert! key-1 key-2 value)
    (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
      (if subtable
          (let ((record (assoc key-2 (cdr subtable) same-key?)))
            (if record
                (set-cdr! record value)
                (set-cdr! subtable
                          (cons (cons key-2 value)
                                (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
    true)
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch))

(define (make-keys-table same-key?)
  (let ((local-table (list '*table*)))

  (define (lookup-iter keys current)
    (let ((record (assoc (car keys) (cdr current) same-key?)))
      (if record
          (if (null? (cdr keys))
              (cdr record)
              (lookup-iter (cdr keys) record))
          false)))
  (define (lookup keys)
    (lookup-iter keys local-table))

  (define (insert-iter! keys value current)
    (let ((record (assoc (car keys) (cdr current) same-key?)))
      (if record
          (if (null? (cdr keys))
              (set-cdr! record value)
              (insert-iter! (cdr keys) value record))
          (if (null? (cdr keys))
              (let ((new-record (cons (cons (car keys) value)
                                      (cdr current))))
                (set-cdr! current new-record))
              (let ((new-record (cons (cons (car keys) '())
                                      (cdr current))))
                (set-cdr! current new-record)
                (insert-iter! (cdr keys) value (car new-record))))))
    true)
  (define (insert! keys value)
    (insert-iter! keys value local-table))

  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          ((eq? m 'local-table) local-table)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch))

(define (entry tree) (car tree))

(define (entry-value tree) (cadr tree))

(define (left-branch tree) (caddr tree))

(define (right-branch tree) (cadddr tree))

(define (set-entry-value! tree value) (set-car! (cdr tree) value))

(define (make-tree entry value left right)
  (list entry value left right))

(define (lookup-tree tree key)
  (cond ((null? tree) false)
        ((= key (entry tree)) tree)
        ((< key (entry tree)) (lookup-tree (left-branch tree) key))
        ((> key (entry tree)) (lookup-tree (right-branch tree) key))))

(define (adjoin-tree tree key value)
  (cond ((null? tree) (make-tree key value '() '()))
        ((= key (entry tree))
         (make-tree (entry tree)
                    value
                    (left-branch tree)
                    (right-branch tree)))
        ((< key (entry tree))
         (make-tree (entry tree)
                    (entry-value tree)
                    (adjoin-tree (left-branch tree) key value)
                    (right-branch tree)))
        ((> key (entry tree))
         (make-tree (entry tree)
                    (entry-value tree)
                    (left-branch tree)
                    (adjoin-tree (right-branch tree) key value)))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (lookup-tree (cdr local-table) key)))
        (if record
            (entry-value record)
            false)))
    (define (insert! key value)
      (let ((record (lookup-tree (cdr local-table) key)))
        (if record
            (set-entry-value! record value)
            (set-cdr! local-table (adjoin-tree (cdr local-table) key value))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (multi-keys-lookup keys table)
  (define (lookup-iter keys current)
    (let ((result ((current 'lookup-proc) (car keys))))
      (if result
          (if (null? (cdr keys))
              result
              (lookup-iter (cdr keys) result))
          false)))
  (lookup-iter keys table))

(define (multi-keys-insert! keys value table)
  (define (insert-iter keys value current)
    (if (null? (cdr keys))
        ((current 'insert-proc!) (car keys) value)
        (let ((result ((current 'lookup-proc) (car keys))))
          (if result
              (insert-iter (cdr keys) value result)
              (let ((new-item (make-table equal?)))
                   ((current 'insert-proc!) (car keys) new-item)
                   (insert-iter (cdr keys) value new-item))))))
  (insert-iter keys value table))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define ta (make-table equal?))