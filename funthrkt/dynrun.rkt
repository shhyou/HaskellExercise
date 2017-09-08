#lang racket

(require syntax/modread)

(provide
 read-module eval-module
 run-dynamic-require)

; read-module : string?  ->  syntax?
(define read-module
  (λ (path)
    (call-with-input-file path
      (λ (port)
        (port-count-lines! port) ; 計算行數而非從檔案開始的字元數
        (with-module-reading-parameterization
            (λ ()
              (read-syntax path port)))))))

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
