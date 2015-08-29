(define-module abs
  (use util.match :only (match))
  (export cps))

(select-module abs)

(define (cps expr)
  (define fresh
    (let ([cnt 1])
      (lambda (s)
        (let ([old-cnt cnt])
          (set! cnt (+ cnt 1))
          ($ string->symbol $ string-append s $ number->string old-cnt)))))
  ; Note: `k` should never duplicate the term it received
  ; since the term is not gaurenteed to be a value in the
  ; presence of `shift` and `reset`
  (define (cpsk expr k)
    (match expr
      [(? (lambda (v) (or (symbol? v) (number? v))) v)
       (k v)]
      [('lambda (x) e)
       (let ([k1 (fresh "k")])
         (k `(lambda (,x ,k1) ,(cpsk e (lambda (v) `(,k1 ,v))))))]
      [('callcc f)
       (cpsk f (lambda (f^)
                 (let* ([k^ (fresh "k")]
                        [k1 (fresh "k")]
                        [tmp (fresh "%")]
                        [v (fresh "%")])
                   `(let ([,k^ (lambda (,tmp) ,(k tmp))])
                      (,f^ (lambda (,v ,k1) (,k^ ,v)) ,k^)))))]
      [('reset e)
       (k (cpsk e (lambda (x) x)))]
      [('shift f)
       (let* ([k1 (fresh "k")]
              [v (fresh "v")]
              [x (fresh "x")])
         (cpsk f (lambda (f^)
                   `(,f^ (lambda (,v ,k1) (,k1 ,(k v))) (lambda (,x) ,x)))))]
      [('if e1 e2 e3)
       (let* ([k1 (fresh "k")]
              [v (fresh "%")])
         `(let ([,k1 (lambda (,v) ,(k v))])
            ,(cpsk e1 (lambda (v)
                        `(if ,v
                             ,(cpsk e2 (lambda (v1) `(,k1 ,v1)))
                             ,(cpsk e3 (lambda (v1) `(,k1 ,v1))))))))]
      [(e1 e2)
       (let ([tmp (fresh "%")])
         (cpsk e1 (lambda (f)
                    (cpsk e2 (lambda (v)
                               `(,f ,v (lambda (,tmp) ,(k tmp))))))))]))
  (cpsk expr (lambda (v) v)))
