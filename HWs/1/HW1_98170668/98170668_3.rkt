#lang racket

(define sign(list (list '+ +) (list '- -) (list '* *) (list '/ /)))

;recursive
(define (main prefix)
 (if (= (length prefix) 1) 
     (car prefix)
     (main (claculate prefix))))

; if correct status (sign number number) calculate
;else go to next one
(define (claculate prefix) 
  (cond ((null? prefix) prefix )
    ((and (assoc (car prefix) sign) 
      (number? (cadr prefix))
      (number? (caddr prefix)))    
     (cons ((cadr (assoc (car prefix) sign) ) (cadr prefix) (caddr prefix))
           (claculate (cdddr prefix))))   
    (else (cons (car prefix) (claculate (cdr prefix))))))




