#lang racket

(define (main l)
  (cond (
         ;empty
         (null? l) '())
        ;if two list make one list
        ((pair? l)
         (append (main (car l)) (main (cdr l))))
        (else (list l))))