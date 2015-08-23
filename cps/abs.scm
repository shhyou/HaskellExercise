(define-module abs
  (define (cpsk expr k)
    (match expr
      [(? (lambda (v) (or (symbol? v) (number? v))) v)
       (k v)]
      [('lambda (x) e)
       #f]
      [('callcc f)
       #f]
      [('reset e)
       #f]
      [('shift f)
       #f]
      [('if e1 e2 e3)
       #f]
      [(e1 e2)
       #f]))
  
  (define (cps expr)
    (cpsk expr (lambda (v) v))))
