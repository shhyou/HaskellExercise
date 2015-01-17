; Modified from Oleg's simple pattern matcher
; http://okmij.org/ftp/Scheme/macros.html#match-case-simple
; Made guards optionsl as in
; https://github.com/yinwang0/old-toys/blob/master/pmatch.scm

; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS Scheme system.
;
; It was first developed for the leanTAP theorem prover in miniKanren.
; It has been in the miniKanren repository
;    http://kanren.sf.net/viewvc/kanren/kanren/mini/leanTAP.scm?view=log
; since August 2005.
;
; See the above code for the example of using match-case-simple:
; transforming a first-order logic formula to the Negation Normal Form.


; (match-case-simple exp <clause> ...[<else-clause>])
; <clause> ::= (<pattern> [<guard>] exp ...)
; <else-clause> ::= (else exp ...)
; <guard> ::= (guard boolean exp)
; <pattern> :: =
;        ,var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         __   -- matches always
;        'exp  -- comparison with exp (using equal?)
;        exp   -- comparison with exp (using equal?)
;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;        (<pattern1> . <pattern2>)  -- ditto
;        ([<pattern>]* ,[var ..]) -- matches the list of patterns and binds the remainings (which must be a list) to var
;        ()    -- matches the empty list

; In the original version, the always-matching pattern was specified
; as a simple underscore. That does not work in R6RS which reserves
; the underscore. Therefore, the always-matching pattern is changed
; to two underscores.

(define-syntax match
  (syntax-rules ()
    ((_ exp clause ...)
      (let ((val-to-match exp))
        (match-case-simple* val-to-match clause ...)))))

(define (match-failure val)
  (error 'match "failed match" val))

(define-syntax match-case-simple*
  (syntax-rules (else guard)
    ((_ val (else exp ...))
      (let () exp ...))
    ((_ val)
      (match-failure val))
    ((_ val (pattern (guard pred) exp ...) . clauses)
      (let ((fail (lambda () (match-case-simple* val . clauses))))
        (match-pattern val pattern
          (if pred (let () exp ...) (fail))
          (fail))))
    ((_ val (pattern exp exp2 ...) . clauses)
      (let ((fail (lambda () (match-case-simple* val . clauses))))
          ; note that match-pattern may do binding. Here,
          ; other clauses are outside of these binding
        (match-pattern val pattern (let () exp exp2 ...) (fail))))
))


; (match-pattern val pattern kt kf)
(define-syntax match-pattern
  (syntax-rules (__ .. quote unquote)
    ((_ val __ kt kf) kt)
    ((_ val () kt kf)
      (if (null? val) kt kf))
    ((_ val (quote lit) kt kf)
      (if (equal? val (quote lit)) kt kf))
    ((_ val (unquote var) kt kf)
      (let ((var val)) kt))
    ((_ val ((unquote (var ..)) . ()) kt kf)
     (if (or (null? val) (pair? val)) (let ((var val)) kt) kf))
    ((_ val (x . y) kt kf)
      (if (pair? val)
        (let ((valx (car val))
              (valy (cdr val)))
          (match-pattern valx x
            (match-pattern valy y kt kf)
            kf))
        kf))
    ((_ val lit kt kf)
      (if (equal? val (quote lit)) kt kf))))

number?

; A simple example:
'(let ()
  (define (test-match x)
    (match-case-simple x
      (,x (number? x) "number")
      (() ()          "nil")
      (#t ()          "bool")
      (#f ()          "bool")
      ((,x . ,y) ()     
        (string-append "pair of " (test-match x) " and " (test-match y)))
      (__ ()           "something else")))
  (for-each (lambda (x) (display (test-match x)) (newline))
    '(1 #t "str" (1 2 3))))
;; printed result:

;; number
;; bool
;; something else
;; pair of number and pair of number and pair of number and nil


; more complex example: meta-circular interpreter

'(let ()
 (define (int code env)
  (match-case-simple code
    (('quote ,x) () x)
    ((let ((,x ,e)) ,body) (symbol? x)
      (let ((xv (int e env)))
        (int body (cons (cons x xv) env))))
    ((lambda () ,body) ()                ; thunk
      (lambda () (int body env)))        ; closed over the env
    ((lambda ,argl ,body) (symbol? argl)  ; arglist
      (lambda arglv
        (int body (cons (cons argl arglv) env))))
    ((lambda (,x) ,body) (symbol? x)      ; 1-arg function
      (lambda (xv)
        (int body (cons (cons x xv) env))))
       ; the general case of lambda is skipped to keep the example small
    ((,op . ,args) ()
      (let* ((opv (int op env))
             (argvs (map (lambda (c) (int c env)) args)))
        (apply opv argvs)))
    (,x (symbol? x) (lookup x env))
    (,x () x)                                ; probably number, string, etc.
 ))

 ; Lookup a symbol in the environment
 (define (lookup x env)
  (cond
    ((assq x env) => cdr)
    (else (error "Can't find " x))))

 ; Initial environment
 (define env0
  (map (lambda (x) (cons x (eval x (interaction-environment))))
    '(+ - display)))                        ; add more

 ; tests

 (int 1 env0)                           ; 1
 (int '1 env0)                                ; '1 is the same as 1

 (int '(quote x) env0)                  ; x

 (int '(display 'x) env0)                ; x

 (int '(display x) env0)                 ; error: unbound x

 (int '(let ((x (+ 1 2 3))) (display x)) env0) ; 6
 ((int '(lambda () 1) env0))                   ; 1
 ((int '(lambda (x) x) env0) 1)                ; 1
 (((int '(lambda (x) (lambda (y) (+ x y))) env0) 2) 3) ; 5 (test closure)

 ((int '(lambda l (display l)) env0) 1 2 3) ; (1 2 3)
)

