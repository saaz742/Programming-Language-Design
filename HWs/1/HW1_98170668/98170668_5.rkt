#lang racket


(define (main n) 
  (maketree n '()))

(define (maketree n tree)
  (cond ((null? n) tree)
        (else (maketree (cdr n )
                              (add (car n) tree)))))

(define (add n tree)
  (cond ((null? tree)
         ;make
         (list n '() '()))
         ;left
        ((<= n (car tree))
         (list (car tree)  
               (add n (cadr tree))
               (caddr tree)))
        ;right
        ((> n (car tree))
         (list (car tree)
               (cadr tree)
               (add n (caddr tree))))))

