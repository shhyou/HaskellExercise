#lang racket/base

(require racket/control)

(define make-permutation
  (λ (xs)
    (let ([cont #f])

      (define (permutation-aux xs acc)
        (cond
         [(null? xs)
          (shift k
             (set! cont k)
             (reverse acc))]
         [else
          (for ([x xs])
            (permutation-aux (remove x xs) (cons x acc)))]))

      (set! cont (lambda ()
                   (reset
                    (permutation-aux xs '())
                    'done)))

      (λ () (cont)))))
