#lang racket
(define-struct queue (head tail len))
 
(define  empty-queue (queue '() '() 0))
 
(define (isempty? q)
  (null? (queue-head q)))

;n = queue size
(define (isfull? q n)
  (equal? (queue-tail q) (+ 1 n)))

(define (enqueue q x)
  (if (null? (queue-head q))
      (queue (reverse (cons x (queue-tail q))) '() (+ (queue-len q) 1))
      (queue (queue-head q) (cons x (queue-tail q))(+ (queue-len q) 1))))


(define (dequeue q)
  (cond [(empty? q) (error "empty")]
        [(not (null? (queue-head q)))
         (if (null? (rest (queue-head q)))
             (queue (reverse (queue-tail q)) '() (- (queue-len q) 1))
             (queue (rest (queue-head q)) (queue-tail q) (- (queue-len q) 1)))]
        [else (queue (reverse (queue-tail q)) '() (- (queue-len q) 1))]))

(define (peek q)
  (cond [(empty? q) (error "empty")]
        [(car (queue-head q))]))


