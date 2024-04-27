#lang racket

(define empty-env
  (lambda () '()))

(define empty?
  (lambda (e)
    (null? e)
    empty-error))

(define has-binding?
  (lambda (envi search-var)
    (letrec ((loop (lambda (env)
                     (cond ((null? env)
                            #f)
                           ((and (pair? env) (pair? (car env)))
                            (let ((saved-var (caar env))
                                  (saved-env (cdr env)))
                              (or (eqv? search-var saved-var) (loop saved-env))))
                           (else
                            (report-invalid-env envi))))))
      (loop envi))))

;env -> variables values
(define union
  (lambda (vars vals env)
    (cond ((and (null? vars) (null? vals))
           env)
          ((and (pair? vars) (pair? vals))
           (union (cdr vars) (cdr vals) (extend-env (car vars) (car vals) env)))
          ((null? vars)
           (no-var))
          (else
           (no-val)))))

;book
(define extend-env
  (lambda (var val e)
    (cons (cons var val) e)))


(define apply-env
  (lambda (env search-var)
                     (cond
                       ((eqv? (car env) 'empty-env)
                            (report-no-binding-found search-var env))
                           ((eqv? (car env) 'extend-env)
                            (let ((saved-var (cadr env))
                                  (saved-val (caddr env))
                                  (saved-env (cadddr env)))
                              (if (eqv? search-var saved-var)
                                  saved-val
                                  (apply-env saved-env search-var))))
                           (else
                            (report-invalid-env env)))))


(define (empty-error)
    (error "not-set-variable"))

(define no-var
  (lambda ()
    (error "no var")))

(define no-val
  (lambda ()
    (error "no value")))

;book
(define report-no-binding-found
  (lambda (search-var env)
    (error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (error 'apply-env "Bad environment ~s" env)))