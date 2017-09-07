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
  (λ (mod name [ns (make-empty-namespace)])
    (namespace-attach-module (current-namespace) ''#%builtin ns)
    (parameterize ([current-namespace ns])
      (namespace-require ''#%kernel)
      (eval mod)
      (dynamic-require name #f))))

(define run-dynamic-require
  (λ (mod id [ns (make-empty-namespace)])
    (namespace-attach-module (current-namespace) ''#%builtin ns)

    (parameterize ([current-namespace ns])
      (dynamic-require mod id))))

(module+ main
  (printf "Dynamic requiring 'hello.rkt'\n")
  (run-dynamic-require "hello.rkt" #f)

  (printf "Dynamic requireing 'intro.rkt'\n")
  (define read-int
    (run-dynamic-require "intro.rkt" 'read-int))

  (printf "read-int from dynamic require ~a\n> " read-int)
  (flush-output)
  (read-int)

  (printf "Reading the module 'hello.rkt'\n")
  (define m (read-module))
  (printf "Evaluating read module, ~a\n" m)
  (eval-module m ''hello))
