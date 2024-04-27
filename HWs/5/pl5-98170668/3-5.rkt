#lang racket
(define value-of
  (lambda (exp env)
    (cases expression exp
           (begin-exp (exp1 exps)
                      (letrec
                          ((value-of-begins
                            (lambda (e1 es)
                              (let ((v1 (value-of e1 env)))
                                (if (null? es)
                                    v1
                                    (value-of-begins (car es) (cdr es)))))))
                        (value-of-begins exp1 exps)))

           (setlist-exp (exps1 exps2 exps3)//list index value
                     (if (null? exps)
                         (list-val '())
                         (list-val
                          (append (exp3 (list (value-of (car exps1) env))
                                  (list (value-of (list-exp (cdr exps1))env))))))
           (newlist-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1))))))
           (get-list-exp (exp exp2)
                     (let ([l (value-of exp env store)])
                     (let ([i (value-of exp2 env (car l))])
                     (list-ref (expval->exp (cadr l))(expval->int (cadr i))))))
      )))


(define the-grammar
  '((program (expression) a-program)
    (expression ("begin" expression (arbno ";" expression) "end")begin-exp)
    [expression ("newlist" "(" expression ")") newlist-exp]
    (expression ("getlist" "(" expression "," expression ")") getlist-exp)
    (expression ("setlist" "(" expression "," expression "," expression ")") setlist-exp)
    ))


(define-datatype expression expression?
  (begin-exp(exp1 expression?)(exps (list-of expression?)))
  (newlist-exp(size number?))
  (getlist-exp(exp1 expression?)(exp2 expression?))
  (setlist-exp(exp1 expression?)(exp2 expression?) (exp3 expression?))
  )

(define expval->list
  (lambda (val)
    (cases expval val
           (bool-val (lst) lst)
           (else (report-expval-extractor-error 'list val)))))