;; language:
;;   e  ::=  n | x | (e1 e2) | (lambda (x) e)
;;       |   (if e1 e2 e3) | (callcc e)

(add-load-path "./" :relative :after)

(load "pp.scm")
(use raw :prefix raw:)
(use simp :prefix simp:)
(use abs :prefix abs:)

(define tests
  '(
    (lambda (x) x)
    (lambda (x) (lambda (y) x))
    (lambda (x) (lambda (y) (f x)))
    (lambda (x)
      (lambda (y)
        (lambda (z)
          ((x z) (y z)))))
    (lambda (f)
      ((lambda (x)
         (f (lambda (v) ((x x) v))))
       (lambda (x)
         (f (lambda (v) ((x x) v))))))
    ((lambda (u) (u u)) (lambda (x) (x x)))
    (add1 (callcc (lambda (esc) (esc 5))))
    ((lambda (x) x)
     (callcc (lambda (f)
               (callcc (lambda (e)
                         (e e))))))
    (if (if (if 0 1 2) 3 4) 5 6)
    (sub1 (reset ((lambda (x) (add1 x)) 5)))
    (reset (mul2
            (reset (add1
                    (shift (lambda (f1)
                             (lambda (f2)
                               (f2 (f1 3)))))))))
    ))

(define (test-all test-proc)
  (pretty-print (map test-proc tests)))

'(
  (define run (lambda () (load "./cps.ss") (test-all test-simp)))
  (run)
  )
