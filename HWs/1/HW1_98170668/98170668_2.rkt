#lang racket

(define (main input output)
  ;replace
  (define outputfile (open-output-file output #:mode 'text #:exists 'replace))
  
  ;faster
  (with-input-from-file input
                  (thunk 
                    (for ([line (in-lines)])
                      (print (eval (read (open-input-string line))) outputfile))))

  ;slower
    ;(for ([line (file->lines input)])
    ;  (print (eval (read (open-input-string line))) outputfile))
  
  ;close
  (close-output-port outputfile))

