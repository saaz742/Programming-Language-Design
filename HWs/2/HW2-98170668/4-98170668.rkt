#lang racket

(define (main tree)
  ;remove node and check right and left recursive
    (cond
       ;empty?
      ((null? tree) +0)
      ((and
        ;both right and left null -> leaf
        (null? (cadr tree))
             (null? (caddr tree))) +1)
        (else(+
         ;right leaves + left leaves = all
         (main (cadr tree))
         (main (caddr tree)))))
 )

