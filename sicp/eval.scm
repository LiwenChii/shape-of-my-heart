;;;;;;;;;;;;;;;;;;;;;;;;;;
(define a 1)
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))
;test
(self-evaluating? 'a)
(self-evaluating? "a")
(self-evaluating? a)

;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (variable? exp) (symbol? exp))
;test
(variable? 'a)

;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
;test
(define q (cons 'quote (cons 2 '())))
q
(define p 4)
p
(quoted? q)
(text-of-quotation q)
(quoted? p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
;test
(define e (cons 'set! (cons 'a (cons 2 '()))))
e
(assignment? e)
(assignment-variable e)
(assignment-value e)


;;;;;;;;;;;;;;;;;;;;;;;;
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
;test
(define e (cons 'define (cons 'foo (cons 2 '()))))
(definition? e)
(definition-variable e)
(definition-value e)
(define e (cons 'define (cons (cons 'foo (cons 'arg1 (cons 'arg2 '()))) (cons '2 '()))))
e
(definition? e)
(definition-value e)
