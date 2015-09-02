(define-module etaview
  (use util.match :only (match))
  (export observe))

(select-module etaview)

(define (observe expr)
  (match expr
    [(? (lambda (v) (or (symbol? v) (number? v))) v)
     v]
    [('lambda (x) (e1 e2))
     (if (eq? x e2)
         (observe e1)
         `(lambda (,x) (,(observe e1) ,(observe e2))))]
    [('lambda (x) e)
     `(lambda (,x) ,(observe e))]
    [('if e1 e2 e3)
     `(if ,(observe e1) ,(observe e2) ,(observe e3))]
    [('callcc f)
     `(callcc ,(observe f))]
    [('reset e)
     `(reset ,(observe e))]
    [('shift f)
     `(shift ,(observe f))]
    [(e1 e2)
     `(,(observe e1) ,(observe e2))]))
