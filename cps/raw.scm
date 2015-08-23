(define-module raw
  (use util.match :only (match)))

(select-module raw)

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
  (cpsk expr))
