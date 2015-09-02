(define-module raw
  (use util.match :only (match))
  (export cps))

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
      [(? (lambda (v) (or (symbol? v) (number? v))) v)
       (let ([k (fresh "k")])
         `(lambda (,k) (,k ,v)))]
      [('lambda (x) e)
       (let* ([k0 (fresh "k")]
              [k1 (fresh "k")])
         `(lambda (k0) (,k0 (lambda (,x)
                              ,(cpsk e)))))]
      [('callcc f)
       (let* ([k (fresh "k")]
              [k1 (fresh "k")]
              [v (fresh "v")])
         `(lambda (,k)
            (,f (lambda (,v)
                  (lambda (,k1)
                    (,k ,v)))
                k)))]
      [('reset e)
       (let ([k (fresh "k")])
         `(lambda (,k)
            (,k (,(cpsk e) (lambda (%0) %0)))))]
      [('shift f)
       (let ([k (fresh "k")]
             [k1 (fresh "k")]
             [v (fresh "v")])
         `(lambda (,k)
            ((,(cpsk f) (lambda (,v)
                          (lambda (,k1)
                            (,k1 (,k ,v)))))
             (lambda (%0) %0))))]
      [('if e1 e2 e3)
       (let* ([k (fresh "k")]
              [v (fresh "v")])
         `(lambda (,k)
            (,(cpsk e1) (lambda (,v)
                          (if ,v
                              (,(cpsk e2) ,k)
                              (,(cpsk e3) ,k))))))]
      [(e1 e2)
       (let ([f (fresh "%")]
             [v (fresh "%")]
             [k (fresh "k")])
         `(lambda (,k)
            (,(cpsk e1)
             (lambda (,f)
               ,(cpsk e2)
               (lambda (,v)
                 ((,f ,v) ,k))))))]))
  (cpsk expr))
