
#lang racket
(require (lib "eopl.ss" "eopl"))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-math-lexer
           (lexer
            (special-keyword  (string->symbol lexeme))
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
            ((:: (:or (:or (:/ #\A #\Z) (:/ #\a #\z)) "_")(:* (:or (:or (:/ #\A #\Z) (:/ #\a #\z)) (:/ #\0 #\9) "_")))(token-ID (string->symbol lexeme)))
            ("true" (token-BOOLEAN #t))
            ("false" (token-BOOLEAN #f))
            ("none" (token-NONE))
            ("and" (string->symbol lexeme))
            ("or" (string->symbol lexeme))
            ("not" (string->symbol lexeme))
            ("+" (token-+))
            ("-" (token--))
            ("*" (token-*))
            ("/" (token-/))
            ("**" (token-**))
            ("==" (token-==))
            ("=" (token-=))
            ("<" (token-<))
            (">" (token->))
            ("[" (token-br))
            ("]" (token-bl))
            ("(" (token-pr))
            (")" (token-pl))
            (":" (token-:))
            ("," (token-c))
            (";" (token-semi))
            ;("in" (string->symbol lexeme))
            ;("if" (token-if))
            ;("def" (token-def))
            ;("else" (token-else))
            ;("print" (token-print))
            ("=" (token-=))
           ; ("" (token-))
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

  (define-lex-abbrevs
    (special-keyword (:or "and"       "or"       "not"  "def"    
                          "break"     "else"      "if"  "in"       
                          "for"      "return"   "continue"
                           "pass"    "global"   
                           "print")))

(define-tokens t (NUM BOOLEAN ID NONE))

(define-empty-tokens et
    (  * **  + : c / -  < = == > br bl pr pl 
      true false none and break continue else for global if  def  in
      not or pass print return EOF semi))

(define simple-math-parser
          (parser
            (start program)
            (end EOF)
            (error void)
            (tokens t et)
            (grammar
         (program
             ((statements)  $1))
        (statements
             ((statement semi) (list $1))
             ((statements statement semi) (cons $2 $1)))
        (statement
             ((compound_stmt) $1)
             ((simple_stmt)  $1))
        (simple_stmt
             ((assignment) $1)
             ((return_stmt)  $1)
             ((global_stmt)  $1)
             ((pass) 'pass )
             ((break) 'break )
             ((continue) 'continue )
             ((print pr atom pl) (list 'print $3)))
        (compound_stmt
             ((function_def)  $1) 
             ((if_stmt) $1)
             ((for_stmt)  $1))
        (assignment
             ((ID = exp) (list `assign $1 $3)))
        (return_stmt
             ((return) (list 'return 'none) ) ;;;
             ((return exp) (list 'return $2))) ;;;
        (global_stmt
             ((global ID) (list $2))) ;;;
        (function_def  ;;;
             ((def ID pr params pl : statements) (list 'fun-def $2 $4 $7))
             ((def ID pr pl : statements) (list 'fun-def $2 (list ) $6)))
        (params ;;;
             ((param_with_defult)  (list $1))
             ((params c param_with_defult) (cons $3 $1)))
        (param_with_defult ;;;
             ((ID = exp) (list $1 $3)))
        (if_stmt
             ((if exp : statements else_block) (list 'if-exp $2 $4 $5)))
        (else_block
             ((else : statements)  $3))
        (for_stmt
             ((for ID in exp : statements) (list 'for-exp $2 $4 $6)))
        (exp
             ((disjunction)  $1))
        (disjunction
             ((conjunction)  $1)
             ((disjunction or conjunction) (list 'or-exp $1 $3)))
       (conjunction
             ((inversion)  $1)
             ((conjunction and inversion) (list 'and-exp $1 $3)))
       (inversion
             ((not inversion) (list 'not-exp $2))
             ((comparison) $1))
       (comparison
             ((sum compare_op_sum_pairs) (append $2 (list $1)))
             ((sum)  $1))
       (compare_op_sum_pairs
             ((compare_op_sum_pair)  $1)
             ((compare_op_sum_pairs compare_op_sum_pair)  $2)) ;;;?
       (compare_op_sum_pair
             ((eq_sum) (list '== $1))
             ((lt_sum) (list '< $1))
             ((gt_sum) (list '> $1)))     
         (eq_sum
             ((== sum)  $2))
         (lt_sum
             ((< sum) $2))
         (gt_sum
             ((> sum)  $2))
         (sum
             ((sum + term) (list '+ $1 $3))
             ((sum - term) (list '- $1 $3))
             ((term) $1))
           (term
             ((term * factor) (list '* $1 $3))
             ((term / factor) (list '/ $1 $3))
             ((factor)  $1 ) )
          (factor
             ((+ factor) (list '++ $2))
             ((- factor) (list '-- $2))
             ((power)  $1))
          (power
             ((atom ** factor) (list '** $1 $3))
             ((primary)  $1))
          (primary
             ((atom)  $1)
             ((primary br exp bl)  (list 'call-arr $1 $3))
             ((primary pr pl)  (list 'call-fun $1 (list ))) ;;;?
             ((primary pr arguments pl)  (list 'call-fun $1 $3)));;;
          (arguments
             ((exp)  (list $1))
             ((arguments c exp) (cons  $3  $1))) ;;;
          (atom
             ((ID)  $1)
             ((true) 'true )
             ((false) 'false )
             ((none) 'none )
             ((NUM)  $1)
             ((list) $1))
         (list
             ((br expressions bl)  $2)
             ((br bl) 'empt-lst))
         (expressions
             ((expressions c exp) (cons  $3  $1))
             ((exp)  (list $1)))
             )))



(define-datatype answer answer?
  [an-answer [val expval?]
             [env environment?]])

(define reference? integer?)
(define the-store 'uninitialized)
(define initialize-store!
  (lambda ()
    (set! the-store `())))

(define setref!
  (lambda (ref val)
    (set! the-store (letrec ([setref-inner (lambda (store1 ref1)
                                             (cond [(null? store1) (report-invalid-reference ref the-store)]
                                                   [(zero? ref1) (cons val (cdr store1))]
                                                   [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))])
                      (setref-inner the-store ref)))))

(define newref
  (lambda (val)
    (let ([next-ref (length the-store)])
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define-datatype environment environment?
  [empty-env]
  [extend-env [var symbol?]
              [val expval?]
              [saved-env environment?]]
  )

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [list-val [mlist list?]]
  [proc-val [proc proc?]]
  [none]
 )

(define-datatype proc proc?
  [procedure [bvars (list-of list?)]
             [body list?]])
             
(define get-value
  (lambda (v)
    (cases expval v
      [num-val (val) val]
      [bool-val (val) val]
      [list-val (val) val]
      [proc-val (val) val]
      [none () `none]
      [else (display `error3)])))

(define (get-ans-value ans)
  (cases answer ans
   [an-answer (val env) val]
   [else (display  `error4)]
  ))
(define (get-ans-env ans)
  (cases answer ans
   [an-answer (val env) env]
   [else (display `error5)]
  ))

(define (get-expval exp)
  (cond
    [(number? exp) (num-val exp)]
    [(boolean? exp) (bool-val exp)]
    [(list? exp) (list-val exp)]
    [(proc? exp) (proc-val exp)]
    [(eqv? `none exp) (none)]
    [else (display `error2)]

    )
  )


(define apply-env
  (lambda (env search-sym)
    (cases environment env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (bvar bval saved-env) (if (eqv? search-sym bvar)
                                            bval
                                            (apply-env saved-env search-sym))]
      )))
(define ret-flag 0)
(define continue-flag 0)
(define break-flag 0)

 (define value-of
  (lambda (exp env)
    ;(newline)
    ;(display "value-of: exp: ")
    ;(display exp )
    ;(display " env: ")
    ;(display env )
    [cond
      [(eqv? exp `none) (an-answer (none) env)]
      [(eqv? exp `pass) (an-answer (none) env)]
      [(eqv? exp `true) (an-answer (bool-val #t) env)]
      [(eqv? exp `false) (an-answer (bool-val #f) env)]
      [(eqv? exp `break) 
            (define anss (an-answer (none) env))
			      (set! break-flag 1)	
			       anss		]
			 [(eqv? exp `continue) 
            (define anss (an-answer (none) env))
			      (set! continue-flag 1)	
			       anss		]
      [(number? exp) (an-answer (num-val exp) env)]
      [(boolean? exp) (an-answer (bool-val exp) env)]
      [(symbol? exp) (an-answer   (apply-env env exp) env) ]
      [(eqv? (car exp) `++) (an-answer (get-ans-value (value-of (cadr exp) env)) env)]
      [(eqv? (car exp) `--) (an-answer 
                                          (get-expval (*
                                          (get-value (get-ans-value (value-of (cadr exp) env)))
                                          -1
                                          )) env)]
      [(eqv? (car exp) `+) 
                                (let (
                                  ( a (get-value (get-ans-value (value-of (cadr exp) env))))
                                  ( b (get-value (get-ans-value (value-of (caddr exp) env))))
                                  )
                                  ( if  (list? a ) 
                                   (an-answer (get-expval (append a b )) env)
                                   (if 
                                   (boolean? a)
                                   (an-answer (get-expval (or a b )) env)
                                   (an-answer (get-expval (+ a b )) env)
                                   )
                                  ))]
                                  
      [(eqv? (car exp) `-) (an-answer (get-expval (-
                                                   (get-value (get-ans-value (value-of (cadr exp) env)))
                                                   (get-value (get-ans-value(value-of (caddr exp) env)))
                                                   )) env)]
      [(eqv? (car exp) `*)
        [cond
        [(eqv? (get-value (get-ans-value (value-of (cadr exp) env))) 0) (an-answer (get-expval 0) env)]
        [(eqv? (get-value (get-ans-value (value-of (cadr exp) env))) 0) (an-answer (get-expval #f) env)]
        [else 
        (let (
                                  ( a (get-value (get-ans-value (value-of (cadr exp) env))))
                                  ( b (get-value (get-ans-value (value-of (caddr exp) env))))
                                  )
                                  [cond
                                  [(eqv? a 0) (an-answer (get-expval 0) env)]
                                  [(eqv? a #f) (an-answer (get-expval #f) env)]
                                  [else 
                                  (if 
                                   (boolean? a)
                                   (an-answer (get-expval (and a b )) env)
                                   (an-answer (get-expval (* a b )) env)
                                   )]
                                  ]
                                  )]]
                                  ]
      
                                                  
                                                  
      [(eqv? (car exp) `/) (an-answer (get-expval (/
                                                   (get-value (get-ans-value (value-of (cadr exp) env)))
                                                   (get-value (get-ans-value (value-of (caddr exp) env)))
                                                   )) env)]
      [(eqv? (car exp) `**) (an-answer (get-expval (expt
                                                    (get-value (get-ans-value (value-of (cadr exp) env)))
                                                    (get-value (get-ans-value (value-of (caddr exp) env)))
                                                    )) env)]
      [(eqv? (car exp) `==) (an-answer (get-expval (equal?
                                                    (get-value (get-ans-value (value-of (cadr exp) env)))
                                                    (get-value (get-ans-value (value-of (caddr exp) env)))
                                                    )) env)]
      [(eqv? (car exp) `<) (an-answer (get-expval (<
                                                   (get-value (get-ans-value (value-of (caddr exp) env)))
                                                   (get-value (get-ans-value (value-of (cadr exp) env)))
                                                   )) env)]
      [(eqv? (car exp) `>) (an-answer (get-expval (>
                                                   (get-value (get-ans-value (value-of (caddr exp) env)))
                                                   (get-value (get-ans-value (value-of (cadr exp) env)))
                                                   )) env)]
      [(eqv? (car exp) `assign) (an-answer (none) (extend-env (cadr exp) (get-ans-value (value-of (caddr exp) env)) env))]

      [(eqv? (car exp) `if-exp)(let(
                                 (exp1 (get-value (get-ans-value (value-of (cadr exp) env))) )
                                 (exp2 (reverse(caddr exp)))
                                 (exp3 (reverse(cadddr exp)))
                                 (anss (an-answer (none) env))
                                 ) 
                                 (filter (lambda (e) (set! anss (value-of e (get-ans-env anss))))  (if exp1 exp2 exp3))
                                 anss
                                 )]

      [(eqv? (car exp) `or-exp)(let(
                                 (exp1 (get-value (get-ans-value (value-of (cadr exp) env))))
                                 (exp2 (get-value (get-ans-value (value-of (caddr exp) env)))))
                       
                                 (an-answer (get-expval (or exp1 exp2)) env) 
                                 )]

      [(eqv? (car exp) `and-exp)(let(
                                 (exp1 (get-value (get-ans-value (value-of (cadr exp) env))))
                                 (exp2 (get-value (get-ans-value (value-of (caddr exp) env)))))
                       
                                 (an-answer (get-expval (and exp1 exp2)) env) 
                                 )]
        [(eqv? (car exp) `not-exp)(let(
                                   (exp1 (get-value (get-ans-value (value-of (cadr exp) env)))))
                         
                                   (an-answer (get-expval (not exp1)) env) 
                                   )]
        ;[(eqv? (car exp) `for-exp)(let(
        ;    (exp2 (get-value (get-ans-value (value-of (caddr exp)env))))
        ;    (body (reverse(cadddr exp)))
        ;    (ret-ans (an-answer (none) env)))
        ;    (for/list ([exp1  exp2])
            ;(filter (lambda (e) (set! anss (value-of e (extend-env (cadr exp) (get-expval exp1) (get-ans-env anss)) )))  exp3)
        ;     (let ([ret-ans (let loop ([body  body])
         ;                                   (if (null? body)
        ;                                       ret-ans
        ;                                      (begin
        ;                                      (set! ret-ans (value-of (car body) (extend-env (cadr exp) (get-expval exp1) (get-ans-env ret-ans))))
        ;                                      (if 
        ;                                       (eqv? continue-flag 1) (begin (set! continue-flag 0) ret-ans)
        ;                                        (if (eqv? break-flag 1) (begin (set! break-flag 0) ret-ans) ; what to do?
        ;                                        (loop (cdr body)))
        ;                                         ))))])
         ;                                           ret-ans)
                                                    
            ;(filter (lambda (e) (set! env (get-ans-env(value-of e (extend-env (cadr exp) (get-expval exp1) env))))) exp3)
         ;   ) ret-ans)]
        [(eqv? (car exp) `for-exp)(let(
            (exp2 (get-value (get-ans-value (value-of (caddr exp)env))))
            (exp3 (reverse (cadddr exp)))
            )
            (set! break-flag 0)
            (set! continue-flag 0)
            (for/list ([exp1  exp2]
                       #:break (eqv? break-flag 1))
            (begin
              (set! continue-flag 0)          
              (filter (lambda (e) (begin
                                    (cond
                                      [(eqv? e `break) (set! break-flag 1)]
                                      [(eqv? e `continue) (set! continue-flag 1)]
                                      [(eqv? (+ break-flag continue-flag) 0) (set! env (get-ans-env(value-of e (extend-env (cadr exp) (get-expval exp1) env))))])
                                    )) exp3)
             ))
            (an-answer (none) env))]
        [(eqv? (car exp) `call-arr) (an-answer (get-expval
                                                (list-ref
                                                   (get-value (get-ans-value (value-of (cadr exp) env)))
                                                   (get-value (get-ans-value(value-of (caddr exp)  env)))
                                                   )
                                                ) env)]
        [(eqv? (car exp) `fun-def) 
            (let 
            ((name (cadr exp))
            (args (reverse (caddr exp)))
            (body (reverse (cadddr exp)))
                  )
            (an-answer (none) (extend-env name  (get-expval (procedure args body)) env))
                                                )]
        [(eqv? (car exp) `call-fun) 
            (let(
            (args (reverse (caddr exp)))
            (proc1 (get-value (get-ans-value (value-of (cadr exp) env))))
            (ret-ans (an-answer (none) env))
            )
            
            (cases proc proc1
                  [procedure (bvars body) 
                  (let ([body-env (let loop ([bvars bvars]
                                             [args args]
                                             [env  env])
                                            (if (null? bvars)
                                               env
                                             (loop (cdr bvars)
                                                 (if (null? args)
                                                     (list )
                                                     (cdr args)
                                                   )
                                                  (extend-env (car (car bvars))
                                                  (if (null? args)
                                                    (get-expval (cadr (car bvars)))
                                                    (get-ans-value (value-of (car args) env))
                                                   )
                                                    env))))])
                                        (set! ret-ans (an-answer (none) body-env))
                                        
                                        
                                        
                                        
                                        (let ([ret-ans (let loop (
                                             [body  body])
                                            (if (null? body)
                                               ret-ans
                                              (begin
                                              (set! ret-ans (value-of (car body) (get-ans-env ret-ans)))
                                              (if 
                                               (eqv? ret-flag 1) (begin (set! ret-flag 0) ret-ans)
                                              (loop (cdr body))
                                                 ))))])
                                                    ret-ans)
                                        
                                        
                                        )
                                        
                                        
                                        ])
            
            )]
                                                
        [(eqv? (car exp) `print)(let(
                                   (exp1 (get-value (get-ans-value (value-of (cadr exp) env)))))
                                   ;(newline)
                                   (display exp1)
                                   (newline)
                                   (an-answer (none) env)
                                   )]
    
        [(eqv? (car exp) `return) 
            (define anss (value-of (cadr exp) env))
			      (set! ret-flag 1)	
			       anss		]
			 
                                   
      
      [else  (an-answer (list-val (reverse exp)) env) ]

     ]
    ))


(define value-of-program
  (lambda (pgm env)
      ;(newline)
      ;(display 'value-of-program )
      ;(display pgm)
    [cond
      [(eqv? (length pgm) 1)  (value-of (car pgm) env)]
      [else (let((renv (get-ans-env (value-of (car pgm) env))))
            (value-of-program (cdr pgm) renv))]
      ]
    ))

(define run
  (lambda (big-list)
    (define big-list2 (reverse big-list))
    (initialize-store!)
    (value-of-program big-list2 (empty-env))
    )
  )



(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors
                "Looking for a ~s, found ~s"
                variant
                value)))


(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref "illegal reference ~s in store ~s" ref the-store)))

;|#
;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "

a = 10;


def f(l = true):
    ls = [];

    for i in [0, 1, 2, 3]:
        def g():
            return a - i;
        ;
        ls = ls + [g];
    ;

    if l :
        return ls;
    else :
        return false;
    ;
;

aa = f();
b = aa[0]();
print(b);
b = aa[1]();
print(b);
b = aa[2]();
print(b);
b = aa[3]();
print(b);
‏
")))

;(let ((parser-res  (simple-math-parser my-lexer))) parser-res)
(let ((parser-res (run (simple-math-parser my-lexer)))) parser-res)



#|

"a =  true; b = -8; if b > -9 :pass; else : print(2);; val = 1; for x in [3,5,7] : val = val+x;; print(val);
 u = [1 , 2] + [3 , 5]; o = true + a; w = a*false ; w2 = 3*3**0/-4.2 >3;"

def g(x = 2,y = 3): print(x);  if x > 0 : return g(x-1); else : return x;; ; x = g(5); print(x);   

|#




