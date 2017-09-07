#lang racket

(provide
 + - * / add1
 (rename-out
  [new-module-begin #%module-begin]
  [new-app #%app]
  [new-datum #%datum])
 #%top-interaction)

(require (for-syntax syntax/parse syntax/srcloc))

(module reader syntax/module-reader
  my-lang)

(define-syntax new-module-begin
  (λ (stx)
    (syntax-parse stx
      [(_ body:expr)
       #'(#%module-begin
          (printf "Evaluation the expression ~a\n" 'body)
          body)]
      [_ (raise-syntax-error 'my-lang "body must be exactly one expression" stx)])))

(define-syntax new-app
  (λ (stx)
    (syntax-parse stx
      [(_ e ...)
       (quasisyntax
        (logging-app
         '(unsyntax (source-location->string stx))
         '(e ...)
         (list (λ () e) ...)))])))

(define-syntax new-datum
  (λ (stx)
    (syntax-parse stx
      [(_ . d:integer)
       (quasisyntax
        (logging-datum
         '(unsyntax (source-location->string stx))
         'd))]
      [_ (raise-syntax-error 'new-datum "only integer datum allowed" stx)])))

(define logging-app
  (λ (location expr thunks)
    (match-define `(,f ,args ...)
      (for/list ([e_i (in-list thunks)])
        (e_i)))
    (define result
      (apply f args))
    (printf "\n~a\n    ~a\n;=> ~a\n;=> ~a\n"
            (~a #:max-width 25 #:limit-prefix? #t #:limit-marker "..." location)
            expr (cons f args) result)
    result))

(define logging-datum
  (λ (location dat)
    (printf "\n~a\n    datum: ~a\n"
            (~a #:max-width 25 #:limit-prefix? #t #:limit-marker "..." location)
            dat)
    dat))
