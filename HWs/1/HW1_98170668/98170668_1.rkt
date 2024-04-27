#lang racket
(define (main l1 l2)
  (for/list ([m1 l1])
    (for/list ([m2 (apply map list l2)])
      (apply + (map * m1 m2)))))
