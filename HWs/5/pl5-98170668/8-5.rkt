#lang racket
(define type-of
  (lambda (exp tenv subst)
    (cases expression exp
      [zero?-exp (exp1) (cases answer (type-of exp1 tenv subst)
                          [an-answer (type1 subst1) (let ([subst2 (unifier type1 (int-type) subst1 exp)])
                                                      (an-answer (bool-type) subst2))])]
      [if-exp (exp1 exp2 exp3)
              (cases answer (type-of exp1 tenv subst)
                [an-answer (ty1 subst) (let ([subst (unifier ty1 (bool-type) subst exp1)])
                                         (cases answer (type-of exp2 tenv subst)
                                           [an-answer (ty2 subst) (cases answer (type-of exp3 tenv subst)
                                                                    [an-answer (ty3 subst) (let ([subst (unifier ty2
                                                                                                                 ty3
                                                                                                                 subst
                                                                                                                 exp)])
                                                                                             (an-answer ty2
                                                                                                        subst))])]))])]
      )))
(define value-of
  (lambda (exp env)
    (cases expression exp
      [zero?-exp (exp1) (let ([val1 (expval->num (value-of exp1 env))])
                          (if (zero? val1)
                              (bool-val #f)
                              (bool-val #t)))]
      [if-exp (exp0 exp1 exp2) (if (expval->bool (value-of exp0 env))
                                   (value-of exp1 env)
                                   (value-of exp2 env))]
      )))

(define expval->bool
  (lambda (v)
    (cases expval v
      [bool-val (bool) bool]
      [num-val (num) (if (equal? num 0) #f #t)]
      [else (expval-extractor-error 'bool v)])))

(define the-grammar
  '([program (expression) a-program]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp])