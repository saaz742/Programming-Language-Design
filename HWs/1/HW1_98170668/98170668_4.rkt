#lang racket

(define input '())

(define (main functions)
   (with-handlers ([exn? (lambda (exn) "error")])
  (and
   (set! input ((car functions)))
   (for ([function (cdr functions)])
     (set! input (function input))) input)
  ))


(define (f)
  (+ 5 0))
(define (g x)
  (* x x))
(define (h x)
  (- x 1))