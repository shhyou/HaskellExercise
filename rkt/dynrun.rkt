#lang racket

(require syntax/modread)

(provide (all-defined-out))

(define read-module
  (λ ()
    (call-with-input-file "hello.rkt"
      (λ (port)
        (port-count-lines! port)
        (with-module-reading-parameterization
            (λ ()
              (read-syntax "hello.rkt" port)))))))

(define eval-module
  (λ (mod name)
    (define ns (make-empty-namespace))
    (namespace-attach-module (current-namespace) ''#%builtin ns)
    (parameterize ([current-namespace ns])
      (namespace-require ''#%kernel)
      (eval mod)
      (dynamic-require name #f))))

(define run-dynamic-require
  (λ ()
    (define ns (make-empty-namespace))
    (namespace-attach-module (current-namespace) ''#%builtin ns)

    (parameterize ([current-namespace ns])
      (dynamic-require "hello.rkt" #f)
      (define read-int
        (dynamic-require "intro.rkt" 'read-int))
      (read-int))))
