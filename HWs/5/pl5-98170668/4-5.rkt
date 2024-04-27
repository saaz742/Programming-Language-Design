#lang racket
(define the-grammar
  '((program (expression) a-program)
    (expression("(" expression expression ")")call-exp)
    (expression("letrec"(arbno identifier "(" identifier ")" "=" expression)"in" expression)letrec-exp)
     ))

  (define value-of
  (lambda (exp env)
    (cases expression exp
           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg (value-of rand env)))
                       (apply-procedure proc arg)))
           (letrec-exp (proc-names bound-vars proc-bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec*
                                  proc-names bound-vars proc-bodies env)))
      )))

  (define-datatype environment environment?
  (empty-env)
  (extend-env(saved-var symbol?) (saved-ref reference?) (saved-env environment?))
  (extend-env-rec*
   (p-names (list-of identifier?))(b-vars (list-of identifier?))(bodies (list-of expression?))(saved-env environment?)))


  
(define-datatype expression expression?
  (call-exp
   (rator expression?)(rand expression?))
  (letrec-exp
   (proc-names (list-of identifier?))(bound-vars (list-of identifier?))(proc-bodies (list-of expression?))(letrec-body expression?))
)