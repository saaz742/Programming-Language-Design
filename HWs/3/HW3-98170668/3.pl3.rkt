#lang racket
(define-struct list (head tail len) #:mutable #:transparent)
(define-struct link (value previous next) #:mutable #:transparent)

(define (empty-doubly-linked-list) (list '() '() 0))

(define (isempty? list)
  (null? (list-head list)))

;n = listsize
(define (isfull? list n)
  (equal? (list-len list) (+ 1 n)))

(define (insert list before after value)
  (define new-link (make-link value before after))
  (if before
      (set-link-next! before new-link)
      (set-list-head! list new-link))
  (if after
      (set-link-previous! after new-link)
      (set-list-tail! list new-link))
  (set-list-len! list (+ (list-len list) 1)) 
    new-link)

(define (insert-before list link value)
  (insert list (link-previous link) link value))

(define (insert-after list link value)
  (insert list link (link-next link) value))

(define ( insert-first list value)
  (insert list #f (list-head list) value))

(define (delete list link)
  (let ((before (link-previous link))
        (after (link-next link)))
    (if before
        (set-link-next! before after)
        (set-list-head! list after))
    (if after
        (set-link-previous! after before)
        (set-list-tail! list before)))
  (set-list-len! list (- (list-len list) 1)) 
  )

(define (delete-first list)
  (delete list (list-head list)))
 

