(load "./pp.scm") ; pretty-print

(define-module raw
  (use util.match :only (match))
  (define (cps expr)
    (define fresh
      (let ([cnt 0])
        (lambda (s)
          (let ([old-cnt cnt])
            (set! cnt (+ cnt 1))
            ($ string->symbol $ string-append s $ number->string old-cnt)))))
    (define (cpsk expr)
      (match expr
        [(? symbol? x)
         (let ([k (fresh "k")])
           `(lambda (,k) (,k ,x)))]
        [('lambda (x) e)
         (let* ([k0 (fresh "k")]
                [k1 (fresh "k")])
           `(lambda (k0) (,k0 (lambda (,x ,k1) (,(cpsk e) ,k1)))))]
        [('callcc f)
         (let* ([k (fresh "k")]
                [k1 (fresh "k")]
                [v (fresh "v")])
           `(lambda (,k)
              (,f (lambda (,v ,k1) (,k ,v)) k)))]
        [(e1 e2)
         (let ([f (fresh "%")]
               [v (fresh "%")]
               [k (fresh "k")])
           `(lambda (,k)
              (,(cpsk e1)
               (lambda (,f)
                 ,(cpsk e2)
                 (lambda (,v)
                   (,f ,v ,k))))))]))
    (cpsk expr)))

;; language:
;;   e  ::=  n | x | (e1 e2) | (lambda (x) e)
;;       |   (if e1 e2 e3) | (callcc e)

(define-module simp
  (use util.match :only (match))
  (define (cps expr)
    (define fresh
      (let ([cnt 1])
        (lambda (s)
          (let ([old-cnt cnt])
            (set! cnt (+ cnt 1))
            ($ string->symbol $ string-append s $ number->string old-cnt)))))
    (define (cont-ap k v)
      (cond [(eq? k 'id) v]
            [(symbol? k) `(,k ,v)]
            [(procedure? k) (k v)]
            [else (error "cont-ap: unknown continuation type")]))
    (define (cont-sym k)
      (cond [(eq? k 'id)
             `(lambda (%0) %0)]
            [(symbol? k) k]
            [(procedure? k)
             (let ([tmp (fresh "%")])
               `(lambda (,tmp) ,(k tmp)))]
            [else (error "cont-sym: unknown continuation type")]))
    (define (cont-trivial? k)
      (cond [(eq? k 'id) #t]
            [(symbol? k) #t]
            [else #f]))
    (define (cpsk expr k)
      (match expr
        [(? (lambda (v) (or (symbol? v) (number? v))) v)
         (cont-ap k v)]
        [('lambda (x) e)
         (let ([k1 (fresh "k")])
           (cont-ap k `(lambda (,x ,k1) ,(cpsk e k1))))]
        [('callcc f)
         (let* ([k1 (fresh "k")]
                [v (fresh "v")])
           (if (cont-trivial? k)
               `(,f (lambda (,v ,k1) ,(cont-ap k v)) ,(cont-sym k))
               (let ([k^ (fresh "k")])
                 `(let ([,k^ ,(cont-sym k)])
                    ,(cpsk f (lambda (f^)
                               `(,f^ (lambda (,v ,k1) (,k^ ,v)) ,k^)))))))]
        [('if e1 e2 e3)
         (if (cont-trivial? k)
             (cpsk e1 (lambda (v)
                        `(if ,v ,(cpsk e2 k) ,(cpsk e3 k))))
             (let ([k1 (fresh "k")])
               `(let ([,k1 ,(cont-sym k)])
                  ,(cpsk e1 (lambda (v)
                              `(if ,v ,(cpsk e2 k1) ,(cpsk e3 k1)))))))]
        [(e1 e2)
         (cpsk e1 (lambda (f)
                    (cpsk e2 (lambda (v)
                               `(,f ,v ,(cont-sym k))))))]))
    (cpsk expr 'id)))

(define (test-raw expr)
  (with-module raw (cps expr)))

(define (test-simp expr)
  (with-module simp (cps expr)))

(define tests
  '(
    (lambda (x) x)
    (lambda (x) (lambda (y) x))
    (lambda (x) (lambda (x) (f x)))
    ((lambda (u) (u u)) (lambda (x) (x x)))
    (add1 (callcc (lambda (esc) (esc 5))))
    ((lambda (x) x)
     (callcc (lambda (f)
               (callcc (lambda (e)
                         (e e))))))
    ))

(define (test-all test-proc)
  (pretty-print (map test-proc tests)))

'(
  (define run (lambda () (load "./cps.ss") (test-all test-simp)))
  (run)
  )
