(define-module simp
  (use util.match :only (match))
  (export cps))

(select-module simp)

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
         (cpsk f (lambda (f^)
                   (let ([build-cont
                          (lambda (k^)
                            `(,f^ (lambda (,v ,k1) ,(cont-ap k^ v)) ,(cont-sym k^)))])
                     (if (cont-trivial? k)
                         (build-cont k)
                         (let ([k^ (fresh "k")])
                           `(let ([,k^ ,(cont-sym k)])
                              ,(build-cont k^))))))))]
      [('reset e)
       `(,(cont-sym k) ,(cpsk e 'id))]
      [('shift f)
       (let* ([k1 (fresh "k")]
              [v (fresh "v")]
              [x (fresh "x")])
         (cpsk f (lambda (f^)
                   `(,f^ (lambda (,v ,k1) (,k1 ,(cont-ap k v))) (lambda (,x) ,x)))))]
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
  (cpsk expr 'id))
