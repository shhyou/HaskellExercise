(load "match-case-simple.ss")

'(lambda (f)
   (lambda (x)
     (f (f x))))

'(lambda (f k)
   (k (lambda (x k)
        (f x (lambda (%1)
               (f %1 k))))))

(define fast-product
  (lambda (xs)
    (call/cc
     (lambda (k)
        (letrec
            ([prod
              (lambda (xs)
                (cond
                 [(null? xs) 1]
                 [(zero? (car xs)) (k 0)]
                 [else
                  (* (car xs) (prod (cdr xs)))]))])
          (prod xs))))))

(define call/cc
  (lambda (f k)
    (f (lambda (v k1)
         (k v))
       k)))

(define fresh
  (let ([cnt -1])
    (lambda (s)
      (set! cnt (+ cnt 1))
      (string->symbol (string-append s (number->string cnt))))))

(define cps-monadic
  (letrec
      ([cps-monadic*
        (lambda (expr)
          (let ([k (fresh "k")])
            (match expr
              [,v (guard (or (integer? v) (symbol? v)))
                  `(lambda (,k) (,k ,v))]
              [(lambda (,x) ,e)
               `(lambda (,x ,k)
                  (,(cps-monadic* e) ,k))]
              [(,e1 ,e2)
               (let* ([v1 (fresh "%")]
                      [v2 (fresh "%")])
                 `(lambda (,k)
                    (,(cps-monadic* e1)
                     (lambda (,v1)
                       (,(cps-monadic* e2)
                        (lambda (,v2)
                          (,v1 ,v2 ,k)))))))]
              [else (error 'cps-monadic* "undefined expression" expr)])))])
    (lambda (expr)
      `(,(cps-monadic* expr) (lambda (v0) v0)))))

(define cpsk
  (letrec
      ([cpsk*
        (lambda (expr k)
          (match expr
            [,v (guard (or (integer? v) (symbol? v)))
                (k v)]
            [(lambda (,x) ,e)
             (let ([k1 (fresh "k")])
               (k `(lambda (,x ,k1)
                     ,(cpsk* e
                             (lambda (v)
                               `(,k1 ,v))))))]
            [(,e1 ,e2)
             (cpsk* e1 (lambda (v1)
                         (cpsk* e2 (lambda (v2)
                                     (let ([tmp (fresh "%")])
                                       `(,v1 ,v2
                                             (lambda (,tmp)
                                               ,(k tmp))))))))]
            [else (error 'cpsk* "undefined expression" expr)]))])
    (lambda (expr)
      (cpsk* expr (lambda (v0) v0)))))

(define cpskt
  (letrec
      ([id (lambda (v0) v0)]
       [cont-ap
        (lambda (k x)
          (cond
           [(procedure? k) (k x)]
           [else `(,k ,x)]))]
       [cont-place
        (lambda (k)
          (cond
           [(procedure? k)
            (let ([tmp (fresh "%")])
              `(lambda (,tmp) ,(k tmp)))]
           [else k]))]
       [cpskt*
        (lambda (expr k)
          (match expr
            [,v (guard (or (integer? v) (symbol? v)))
                (cont-ap k v)]
            [(lambda (,x) ,e)
             (let ([k1 (fresh "k")])
               (cont-ap k `(lambda (,x ,k1)
                             ,(cpskt* e k1))))]
            [(,e1 ,e2)
             (cpskt* e1 (lambda (v1)
                          (cpskt* e2 (lambda (v2)
                                       `(,v1 ,v2 ,(cont-place k))))))]
            [else (error 'cpskt* "undefined expression" expr)]))])
    (lambda (expr)
      (cpskt* expr id))))

'(
  5
  (lambda (f) (f 5))
  (cps-monadic '(lambda (f) (f 5)))
  (cpsk '(lambda (f) (f 5)))
  (cpskt '(lambda (f) (f 5)))
  
  
  (lambda (f) (lambda (x) (f (f x))))
  (cps-monadic '(lambda (f) (lambda (x) (f (f x)))))
  (cpsk '(lambda (f) (lambda (x) (f (f x)))))
  (cpskt '(lambda (f) (lambda (x) (f (f x)))))
  )
