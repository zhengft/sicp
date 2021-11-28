(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
             (right (eval (rest-operands exps) env)))
        (cons left right))))

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (eval (rest-operands exps) env))
            (left (eval (first-operand exps) env)))
        (cons left right))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

; definitions
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

; lambda expressions
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; conditionals
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin - sequencing
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; application related selectors and predicate
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

; cond, can be represented as nested ifs
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;; eval-rules is an association list (1d table) of
        ;; 'expression type'-'evaluation rule' pairs.
        ;; expression type is a symbol ('quote, 'define, 'lambda etc.)
        ;; evaluation rule must be a procedure of two arguments, exp and env.
        ;; defined at the end of file.
        ((assq (car exp) eval-rules) => (lambda (type-rule-pair)
                                          ((cdr type-rule-pair) exp env)))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
 
(define eval-rules
  (list (cons 'quote (lambda (exp env) (text-of-quotation exp)))
        (cons 'set! eval-assignment)
        (cons 'define eval-definition)
        (cons 'if eval-if)
        (cons 'lambda (lambda (exp env)
                        (make-procedure (lambda-parameters exp)
                                        (lambda-body exp)
                                        env)))
        (cons 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
        (cons 'cond (lambda (exp env) (eval (cond->if exp) env)))
        (cons 'and eval-and)
        (cons 'or eval-or)
  ))

(define (get-conditions exp) (cdr exp))

(define (eval-and exp env)
  (define (eval-conditions conditions env)
    (if (null? conditions)
        'true
        (if (eval (car conditions) env)
            (eval-conditions (cdr conditions) env)
            'false)))

  (eval-conditions (get-conditions exp) env))

(define (eval-or exp env)
  (define (eval-conditions conditions env)
    (if (null? conditions)
        'false
        (if (eval (car conditions) env)
            'true
            (eval-conditions (cdr conditions) env))))

  (eval-conditions (get-conditions exp) env))

(define (and->if exp)
  (expand-and-conditions (get-conditions exp)))

(define (expand-and-conditions conditions)
  (if (null? conditions)
      'true
      (let ((first (car conditions))
            (rest (cdr conditions)))
        (make-if first
                 (expand-and-conditions rest)
                 'false))))

(define (or->if exp)
  (expand-or-conditions (get-conditions exp)))

(define (expand-or-conditions conditions)
  (if (null? conditions)
      'false
      (let ((first (car conditions))
            (rest (cdr conditions)))
        (make-if first
                 'true
                 (expand-and-conditions rest)))))
