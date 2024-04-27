#lang racket
(define value-of
  (lambda (exp env)
    (cases expression exp
           (newref-exp (exp1)
                       (let ((v1 (value-of exp1 env)))
                         (ref-val (newref v1))))
           (deref-exp (exp1)
                      (let ((v1 (value-of exp1 env)))
                        (let ((ref1 (expval->ref v1)))
                          (deref ref1))))
           (setref-exp (exp1 exp2)
                       (let ((ref (expval->ref (value-of exp1 env))))
                         (let ((val2 (value-of exp2 env)))
                           (begin
                             (setref! ref val2)
                             val2))))

      )))

(define the-grammar
  '((program (expression) a-program)
    (expression("newref" "(" expression ")")newref-exp)
    (expression ("deref" "(" expression ")")deref-exp)
    (expression("setref" "(" expression "," expression ")")setref-exp)
    ))

(define-datatype expval expval?
  (ref-val
   (ref reference?))

(define-datatype expression expression?
  (newref-exp(exp1 expression?))
  (deref-exp(exp1 expression?))
  (setref-exp
   (exp1 expression?)
   (exp2 expression?)))

(define expval->ref
  (lambda (val)
    (cases expval val
           (ref-val (ref) ref)
           (else (report-expval-extractor-error 'reference val)))))