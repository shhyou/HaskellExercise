#lang racket

(define (big5!)
  (current-output-port
   (reencode-output-port (current-output-port) "big-5")))

;
; t ::= U
;    |  (t → t)
;    |  (Π(x : t1) t2)
;    |  (Σ(x : t1) t2)
;    |  (t1 :+: t2)
;    |  (e1 = t e2)
;
; e ::= x
;    |  (e : t)
;    |  (let x : t = e1
;        e2)
;    |  λ x e
;    |  (e1 e2)
;    |  (e1 |,| e2)
;    |  (ind p (z => e1)
;            ((x |,| y) => e2))
;    |  (inl e)
;    |  (inr e)
;    |  (ind p (z => e1)
;            ((inl a) => e2)
;            ((inr b) => e3))
;    |  (refl e)
;    |  (ind p ((p : x = y) => e1)
;            ((refl z) => e2))
;

(struct construct
  (name
   syntax
   type-infer
   type-check
   evaluate)
  #:transparent)
; name : string
; syntax : mkstx
; type-infer : construct → (var, type) dict → expr → type
; type-check : construct → (var, type) dict → expr → type → unit
; evaluate : construct → expr → expr


(struct mkstx (vars terms bindings instanceof instantiate build check subst equal) #:transparent)
; instanceof : expr → bool
; instantiate : expr → (var, expr) dict
; build : (var, expr) dict → expr
; check : mkstx → var set → expr → bool
; subst : mkstx → (var, expr) dict → expr → expr
; equal : mkstx → (var, expr) dict → expr → (var, expr) dict → expr → bool

(define (make-syntax vars terms bindings stx)
  (define (mk-isinstance ss)
    (cond [(null? ss) null?]
          [(pair? ss)
           (let ([inst-car (mk-isinstance (car ss))]
                 [inst-cdr (mk-isinstance (cdr ss))])
             (λ (xs) (and (pair? xs) (inst-car (car xs)) (inst-cdr (cdr xs)))))]
          [(or (set-member? vars ss) (set-member? terms ss)) (λ (_) #t)]
          [else (λ (xs) (eq? xs ss))]))
  (define (mk-builder ss)
    (cond [(null? ss) (λ (inst) '())]
          [(pair? ss)
           (let ([build-car (mk-builder (car ss))]
                 [build-cdr (mk-builder (cdr ss))])
             (λ (inst) (cons (build-car inst) (build-cdr inst))))]
          [(or (set-member? vars ss) (set-member? terms ss))
           (λ (inst) (dict-ref inst ss))]
          [else (λ (inst) ss)]))
  (define (mk-instantiate ss)
    (cond
     [(null? ss) (λ (inst xs) (and (null? xs) inst))]
     [(pair? ss)
      (let ([inst-car (mk-instantiate (car ss))]
            [inst-cdr (mk-instantiate (cdr ss))])
        (λ (inst xs)
          (and (pair? xs)
            (let ([inst/car (inst-car inst (car xs))])
              (and inst/car (inst-cdr inst/car (cdr xs)))))))]
     [(set-member? vars ss)
      (λ (inst xs) (and (symbol? xs) (dict-set inst ss xs)))]
     [(set-member? terms ss)
      (λ (inst xs) (dict-set inst ss xs))]
     [else (λ (inst xs) (and (eq? ss xs) inst))]))
  (let ([do-instantiate (mk-instantiate stx)]
        [do-build (mk-builder stx)])
    (define this-instanceof (mk-isinstance stx))
    (define (this-instantiate xs) (do-instantiate (hasheq) xs))
    (define (this-build inst) (do-build inst))
    (define (this-check stx cxt xs)
      (let ([inst (this-instantiate xs)])
        (and inst
         (for/and ([term terms])
           ((mkstx-check stx) stx
            (for/fold ([acc cxt]) ([x (dict-ref bindings term (λ () '()))])
              (set-add acc (dict-ref inst x)))
            (dict-ref inst term))))))
    ; substitute x for e1 in e2
    ;   x      [e1 / x] = e1
    ;   y      [e1 / x] = y                           (x /= y)
    ;   (e2 e3)[e1 / x] = (e2[e1 / x]) (e3[e1 / x])
    ;   λy.e2  [e1 / x] = λz.(e2[z/y])[e1/x]
    (define (this-subst stx subst e)
      (let*
        ([inst (this-instantiate e)]
         [inst/fresh (for/fold ([acc inst]) ([var vars])
                      (let ([var/inst (dict-ref inst var)])
                        (dict-set acc var (gensym var/inst))))]
         [subst/fresh (for/fold ([acc subst]) ([var vars])
                       (let ([var/inst (dict-ref inst var)]
                             [var/fresh (dict-ref inst/fresh var)])
                         (dict-set acc var/inst var/fresh)))]
         [inst/subexpr (for/fold ([acc inst/fresh]) ([term terms])
                        (let ([term/inst (dict-ref inst term)])
                          (dict-set acc term
                           ((mkstx-subst stx) stx subst/fresh term/inst))))])
        (do-build inst/subexpr)))
    (define (this-equal stx subst1 e1 subst2 e2)
      (let ([inst/fresh (for/hash ([var vars]) (values var (gensym var)))]
            [inst1 (this-instantiate e1)]
            [inst2 (this-instantiate e2)])
        (let ([subst1/fresh (for/fold ([acc subst1]) ([var vars])
                             (let ([var/inst (dict-ref inst1 var)]
                                   [var/fresh (dict-ref inst/fresh var)])
                               (dict-set acc var/inst var/fresh)))]
              [subst2/fresh (for/fold ([acc subst2]) ([var vars])
                             (let ([var/inst (dict-ref inst2 var)]
                                   [var/fresh (dict-ref inst/fresh var)])
                               (dict-set acc var/inst var/fresh)))])
          (for/and ([term terms])
           ((mkstx-equal stx) stx
            subst1/fresh (dict-ref inst1 term)
            subst2/fresh (dict-ref inst2 term))))))
    (mkstx vars terms bindings this-instanceof this-instantiate this-build this-check this-subst this-equal)))

(define (make-cong-evaluator stx)
  (λ (conss e)
    (let ([inst ((mkstx-instantiate stx) e)])
      (define inst/evaluated
        (for/fold ([acc inst]) ([term (mkstx-terms stx)])
          (let ([term/nf ((construct-evaluate conss) conss (dict-ref inst term))])
            (dict-set acc term term/nf))))
      ((mkstx-build stx) inst/evaluated))))

(define-struct (type-infer-failed exn:fail) (ctxt expr) #:transparent)
(define-struct (type-check-failed exn:fail) (ctxt expr type/actual type/expected) #:transparent)
(define-struct (eval-error exn:fail) (expr) #:transparent)

(define (bidir-inferable conss cxt e t)
  (let ([t/infer ((construct-type-infer conss) conss cxt e)])
;    (display (format "bidir:check->infer\n\ncxt ~a\n\ne ~a\nt ~a\ninfer ~a\n\n" cxt e t t/infer))
    (let ([t/nf ((construct-evaluate conss) conss t)]
          [t/infer/nf ((construct-evaluate conss) conss t/infer)])
      (unless (stxequal t/nf t/infer/nf)
        (raise (type-check-failed "type mismatch" (current-continuation-marks)
                                  cxt e t t/infer))))))

(define (bidir-cannot-infer additional-msg)
  (λ (conss cxt e)
    (raise (type-infer-failed (string-append "cannot infer (" additional-msg ")")
                              (current-continuation-marks) cxt e))))

(define stx/anno
  (make-syntax '() '(e t) '()
   '(e : t)))

(define anno
  (construct "_:_"
   ; syntax
   stx/anno
   ; type-infer
   (λ (conss cxt e)
     (let ([inst ((mkstx-instantiate stx/anno) e)])
       ((construct-type-check conss) conss
        cxt (dict-ref inst 'e) (dict-ref inst 't))
       ; TODO: normalize the type
       (dict-ref inst 't)))
   ; type-check
   bidir-inferable
   ; evaluate
   (λ (conss e)
     (let ([inst ((mkstx-instantiate stx/anno) e)])
       ((construct-evaluate conss) conss
        (dict-ref inst 'e))))
   ))

(define stx/let
  (make-syntax '(x) '(t e1 e2) '((e2 . (x)))
    '(let x : t = e1

      e2)))

(define (let/preprocess conss cxt e)
  (let ([inst ((mkstx-instantiate stx/let) e)])
    ((construct-type-check conss) conss
     cxt (dict-ref inst 'e1) (dict-ref inst 't))
    (stxsubst
     (dict-ref inst 'e2)
     ((mkstx-build stx/anno)
      (hasheq 'e (dict-ref inst 'e1) 't (dict-ref inst 't)))
     (dict-ref inst 'x))))

(define let/subst
  (construct "let"
   ; syntax
   stx/let
   ; type-infer
   (λ (conss cxt e)
     ((construct-type-infer conss) conss
      cxt (let/preprocess conss cxt e)))
   ; type-check
   (λ (conss cxt e t)
     ((construct-type-check conss) conss
      cxt (let/preprocess conss cxt e) t))
   ; evaluate
   (λ (conss e)
     (let* ([inst ((mkstx-instantiate stx/let) e)]
            [e1/nf ((construct-evaluate conss) conss (dict-ref inst 'e1))])
       ((construct-evaluate conss) conss
        (stxsubst (dict-ref inst 'e2) (dict-ref inst 'e1) (dict-ref inst 'x)))))
   ))

(define type/U
  (construct "U"
   ; syntax
   (make-syntax '() '() '()
    'U)
   ; type-infer
   (λ (conss cxt e) 'U)
   ; type-check
   bidir-inferable
   ; evaluate
   (λ (conss e) 'U)
   ))

(define stx/->
  (make-syntax '() '(t1 t2) '()
   '(t1 → t2)))

(define type/->
  (construct "_→_"
   ; syntax
   stx/->
   ; type-infer
   (λ (conss cxt e)
     (let ([inst ((mkstx-instantiate stx/->) e)])
       ((construct-type-check conss) conss cxt (dict-ref inst 't1) 'U)
       ((construct-type-check conss) conss cxt (dict-ref inst 't2) 'U)
       'U))
   ; type-check
   bidir-inferable
   ; evaluate
   (make-cong-evaluator stx/->)
   ))

(define stx/Pi
  (make-syntax '(x) '(A B) '((B . (x)))
   '(Π (x : A) B)))

(define type/Pi
  (construct "(Π(x : A) B)"
   ; syntax
   stx/Pi
   ; type-infer
   (λ (conss cxt e)
     (let ([inst ((mkstx-instantiate stx/Pi) e)])
       ((construct-type-check conss) conss
        cxt (dict-ref inst 'A) 'U)
       ((construct-type-check conss) conss
        (dict-set cxt (dict-ref inst 'x) (dict-ref inst 'A))
        (dict-ref inst 'B)
        'U)
       'U))
   ; type-check
   bidir-inferable
   ; evaluate
   (make-cong-evaluator stx/Pi)
   ))

(define stx/fn/abs
  (make-syntax '(x) '(e) '((e . (x)))
   '(λ x e)))

(define fn/abs
  (construct "(λ x e)"
   ; syntax
   stx/fn/abs
   ; type-infer
   (bidir-cannot-infer "fn/abs")
   ; type-check
   (λ (conss cxt e t)
     (let ([inst/e ((mkstx-instantiate stx/fn/abs) e)]
           [t/nf ((construct-evaluate conss) conss t)])
       (cond
        [((mkstx-instanceof stx/->) t/nf)
         (let ([inst/t ((mkstx-instantiate stx/->) t/nf)])
           ((construct-type-check conss) conss
            (dict-set cxt (dict-ref inst/e 'x) (dict-ref inst/t 't1))
            (dict-ref inst/e 'e)
            (dict-ref inst/t 't2)))]
        [((mkstx-instanceof stx/Pi) t/nf)
         (let ([inst/t ((mkstx-instantiate stx/Pi) t/nf)]
               [x (gensym (dict-ref inst/e 'x))])
           ((construct-type-check conss) conss
            (dict-set cxt x (dict-ref inst/t 'A))
            (stxsubst (dict-ref inst/e 'e) x (dict-ref inst/e 'x))
            (stxsubst (dict-ref inst/t 'B) x (dict-ref inst/t 'x))))]
        [else
         (raise (type-check-failed "not function type" (current-continuation-marks)
                                   cxt e t '(or (? → ?) (Π(? : ?) ?))))])))
   ; evaluate
   (make-cong-evaluator stx/fn/abs)
   ))

(define stx/fn/ap
  (make-syntax '() '(e1 e2) '()
   '(e1 e2)))

(define fn/ap
  (construct "(e1 e2)"
   ; syntax
   stx/fn/ap
   ; type-infer
   (λ (conss cxt e)
     (let*
       ([inst/e ((mkstx-instantiate stx/fn/ap) e)]
        [t1 ((construct-type-infer conss) conss cxt (dict-ref inst/e 'e1))]
        [t1/nf ((construct-evaluate conss) conss t1)])
       (cond
        [((mkstx-instanceof stx/->) t1/nf)
         (let ([inst/A->B ((mkstx-instantiate stx/->) t1/nf)])
           ((construct-type-check conss) conss
            cxt
            (dict-ref inst/e 'e2)
            (dict-ref inst/A->B 't1))
           (dict-ref inst/A->B 't2))]
        [((mkstx-instanceof stx/Pi) t1/nf)
         (let ([inst/PiAB ((mkstx-instantiate stx/Pi) t1/nf)])
           ((construct-type-check conss) conss
            cxt
            (dict-ref inst/e 'e2)
            (dict-ref inst/PiAB 'A))
           (stxsubst (dict-ref inst/PiAB 'B)
                     (dict-ref inst/e 'e2) (dict-ref inst/PiAB 'x)))]
        [else
         (raise (type-infer-failed "not _→_ or Π-type" (current-continuation-marks)
                                   cxt e))])))
   ; type-check
   bidir-inferable
   ; evaluate
   (let ([cong-evaluator (make-cong-evaluator stx/fn/ap)])
     (λ (conss e)
       (let*
         ([e1e2/nf (cong-evaluator conss e)]
          [inst ((mkstx-instantiate stx/fn/ap) e1e2/nf)]
          [inst/e1 ((mkstx-instantiate stx/fn/abs) (dict-ref inst 'e1))])
         (if inst/e1
          ((construct-evaluate conss) conss
           (stxsubst (dict-ref inst/e1 'e) (dict-ref inst 'e2) (dict-ref inst/e1 'x)))
          e1e2/nf))))
   ))

(define stx/Sigma
  (make-syntax '(x) '(A B) '((B . (x)))
   '(Σ(x : A) B)))

(define type/Sigma
  (construct "Σ(x : A) B"
   ; syntax
   stx/Sigma
   ; type-infer
   (λ (conss cxt e)
     (let ([inst ((mkstx-instantiate stx/Sigma) e)])
       ((construct-type-check conss) conss
        cxt (dict-ref inst 'A) 'U)
       ((construct-type-check conss) conss
        (dict-set cxt (dict-ref inst 'x) (dict-ref inst 'A))
        (dict-ref inst 'B)
        'U)
       'U))
   ; type-check
   bidir-inferable
   ; evaluate
   (make-cong-evaluator stx/Sigma)
   ))

(define stx/pair/cons
  (make-syntax '() '(x y) '()
   '(x |,| y)))


(define pair/cons
  (construct "_,_"
   ; syntax
   stx/pair/cons
   ; type-infer
   (bidir-cannot-infer "pair/cons")
   ; type-check
   (λ (conss cxt e t)
     (let ([inst/e ((mkstx-instantiate stx/pair/cons) e)]
           [inst/t ((mkstx-instantiate stx/Sigma)
                    ((construct-evaluate conss) conss t))])
       (unless inst/t
         (raise (type-check-failed "not Σ-type" (current-continuation-marks)
                                   cxt e t '(Σ(? : ?) ?))))
       ((construct-type-check conss) conss
        cxt (dict-ref inst/e 'x) (dict-ref inst/t 'A))
       ((construct-type-check conss) conss
        cxt
        (dict-ref inst/e 'y)
        (stxsubst (dict-ref inst/t 'B) (dict-ref inst/e 'x) (dict-ref inst/t 'x)))))
   ; evaluate
   (make-cong-evaluator stx/pair/cons)
   ))

(define stx/pair/ind
  (make-syntax '(z x y) '(p C g) '((C . (z)) (g . (x y)))
    '(ind p (z => C) ((x |,| y) => g))))

(define pair/ind
  (construct "U"
   ; syntax
   stx/pair/ind
   ; type-infer
   (λ (conss cxt e)
     (let* ([inst/e ((mkstx-instantiate stx/pair/ind) e)]
            [t/p ((construct-type-infer conss) conss cxt (dict-ref inst/e 'p))]
            [t/p/nf ((construct-evaluate conss) conss t/p)]
            [inst/t/p ((mkstx-instantiate stx/Sigma) t/p/nf)])
       (unless inst/t/p
         (raise (type-check-failed "not Σ-type" (current-continuation-marks)
                                   cxt (dict-ref inst/e 'p) t/p '(Σ(? : ?) ?))))
       (let ([z (dict-ref inst/e 'z)]
             [C (dict-ref inst/e 'C)]
             [x (dict-ref inst/e 'x)]
             [y (dict-ref inst/e 'y)])
         ((construct-type-check conss) conss
          (dict-set cxt z t/p/nf)
          C 'U)
         ((construct-type-check conss) conss
          (dict-set* cxt
                     x (dict-ref inst/t/p 'A)
                     y (stxsubst (dict-ref inst/t/p 'B) x (dict-ref inst/t/p 'x)))
          (dict-ref inst/e 'g)
          (stxsubst C
                    ((mkstx-build stx/pair/cons)
                     (hasheq 'x x 'y y))
                    z))
         (stxsubst C (dict-ref inst/e 'p) z))))
   ; type-check
   bidir-inferable
   ; evaluate
   (let ([cong-evaluator (make-cong-evaluator stx/pair/ind)])
     (λ (conss e)
       (let* ([eP/nf (cong-evaluator conss e)]
              [inst/e ((mkstx-instantiate stx/pair/ind) eP/nf)]
              [inst/p ((mkstx-instantiate stx/pair/cons) (dict-ref inst/e 'p))])
          (if inst/p
           (let ([x (dict-ref inst/e 'x)] [y (dict-ref inst/e 'y)]
                 [x* (dict-ref inst/p 'x)] [y* (dict-ref inst/p 'y)])
             ((construct-evaluate conss) conss
              (stxsubst (stxsubst (dict-ref inst/e 'g) x* x) y* y)))
           eP/nf))))
   ))

(define stx/coproduct
  (make-syntax '() '(A B) '()
    '(A :+: B)))

(define type/coproduct
  (construct "A :+: B"
   ; syntax
   stx/coproduct
   ; type-infer
   (λ (conss cxt e)
     (let ([inst ((mkstx-instantiate stx/coproduct) e)])
       ((construct-type-check conss) conss
        cxt (dict-ref inst 'A) 'U)
       ((construct-type-check conss) conss
        cxt (dict-ref inst 'B) 'U)
       'U))
   ; type-check
   bidir-inferable
   ; evaluate
   (make-cong-evaluator stx/coproduct)
   ))

(define stx/coproduct/inl
  (make-syntax '() '(e) '()
    '(inl e)))

(define coproduct/inl
  (construct "inl _"
   ; syntax
   stx/coproduct/inl
   ; type-infer
   (bidir-cannot-infer "coproduct/inl")
   ; type-check
   (λ (conss cxt e t)
     (let ([inst/e ((mkstx-instantiate stx/coproduct/inl) e)]
           [inst/t ((mkstx-instantiate stx/coproduct)
                    ((construct-evaluate conss) conss t))])
       (unless inst/t
         (raise (type-check-failed "not :+:-type" (current-continuation-marks)
                                   cxt e t '(? :+: ?))))
       ((construct-type-check conss) conss
        cxt (dict-ref inst/e 'e) (dict-ref inst/t 'A))))
   ; evaluate
   (make-cong-evaluator stx/coproduct/inl)
   ))

(define stx/coproduct/inr
  (make-syntax '() '(e) '()
    '(inr e)))

(define coproduct/inr
  (construct "inr _"
   ; syntax
   stx/coproduct/inr
   ; type-infer
   (bidir-cannot-infer "coproduct/inr")
   ; type-check
   (λ (conss cxt e t)
     (let ([inst/e ((mkstx-instantiate stx/coproduct/inr) e)]
           [inst/t ((mkstx-instantiate stx/coproduct)
                    ((construct-evaluate conss) conss t))])
       (unless inst/t
         (raise (type-check-failed "not :+:-type" (current-continuation-marks)
                                   cxt e t '(? :+: ?))))
       ((construct-type-check conss) conss
        cxt (dict-ref inst/e 'e) (dict-ref inst/t 'B))))
   ; evaluate
   (make-cong-evaluator stx/coproduct/inr)
   ))

(define stx/coproduct/ind
  (make-syntax '(z a b) '(c C f g) '((C . (z)) (f . (a)) (g . (b)))
    '(ind c (z => C)
            ((inl a) => f)
            ((inr b) => g))))

(define coproduct/ind
  (construct "coproduct/ind"
   ; syntax
   stx/coproduct/ind
   ; type-infer
   (λ (conss cxt e)
     (let* ([inst/e ((mkstx-instantiate stx/coproduct/ind) e)]
            [t/c ((construct-type-infer conss) conss cxt (dict-ref inst/e 'c))]
            [inst/t/c ((mkstx-instantiate stx/coproduct)
                       ((construct-evaluate conss) conss t/c))])
       (unless inst/t/c
         (raise (type-check-failed "not :+:-type" (current-continuation-marks)
                                   cxt (dict-ref inst/e 'c) t/c '(? :+: ?))))
       (let ([a (dict-ref inst/e 'a)] [b (dict-ref inst/e 'b)] [z (dict-ref inst/e 'z)])
         (define (make-C stx z*)
           (stxsubst (dict-ref inst/e 'C)
            (if stx ((mkstx-build stx) (hasheq 'e z*)) z*) z))
         ((construct-type-check conss) conss
          (dict-set cxt z t/c)
          (dict-ref inst/e 'C) 'U)
         ((construct-type-check conss) conss
          (dict-set cxt a (dict-ref inst/t/c 'A))
          (dict-ref inst/e 'f)
          (make-C stx/coproduct/inl a))
         ((construct-type-check conss) conss
          (dict-set cxt b (dict-ref inst/t/c 'B))
          (dict-ref inst/e 'g)
          (make-C stx/coproduct/inr b))
         (make-C #f (dict-ref inst/e 'c)))))
   ; type-check
   bidir-inferable
   ; evaluate
   (let ([cong-evaluator (make-cong-evaluator stx/coproduct/ind)])
    (λ (conss e)
      (let* ([eC/nf (cong-evaluator conss e)]
             [inst/e ((mkstx-instantiate stx/coproduct/ind) eC/nf)]
             [inst/cl ((mkstx-instantiate stx/coproduct/inl) (dict-ref inst/e 'c))]
             [inst/cr ((mkstx-instantiate stx/coproduct/inr) (dict-ref inst/e 'c))])
        (cond
         [inst/cl
          ((construct-evaluate conss) conss
           (stxsubst (dict-ref inst/e 'f) (dict-ref inst/cl 'e) (dict-ref inst/e 'a)))]
         [inst/cr
          ((construct-evaluate conss) conss
           (stxsubst (dict-ref inst/e 'g) (dict-ref inst/cr 'e) (dict-ref inst/e 'b)))]
         [else eC/nf]))))
   ))

(define stx/Id
  (make-syntax '() '(x A y) '()
   '(x = A y)))

(define type/Id
  (construct "_=_"
   ; syntax
   stx/Id
   ; type-infer
   (λ (conss cxt e)
     (let ([inst ((mkstx-instantiate stx/Id) e)])
       ((construct-type-check conss) conss
        cxt (dict-ref inst 'A) 'U)
       ((construct-type-check conss) conss
        cxt (dict-ref inst 'x) (dict-ref inst 'A))
       ((construct-type-check conss) conss
        cxt (dict-ref inst 'y) (dict-ref inst 'A))
       'U))
   ; type-check
   bidir-inferable
   ; evaluate
   (make-cong-evaluator stx/Id)
   ))

(define stx/id/refl
  (make-syntax '() '(a) '()
   '(refl a)))

(define id/refl
  (construct "refl"
   ; syntax
   stx/id/refl
   ; type-infer
   (λ (conss cxt e)
     (let* ([inst ((mkstx-instantiate stx/id/refl) e)]
            [a (dict-ref inst 'a)]
            [A ((construct-type-infer conss) conss cxt a)])
       ; possibly TODO: normalize A and a
       ((mkstx-build stx/Id)
        (hasheq 'A A 'x a 'y a))))
   ; type-check
   (λ (conss cxt e t)
     (let ([inst/e ((mkstx-instantiate stx/id/refl) e)]
           [inst/t ((mkstx-instantiate stx/Id)
                    ((construct-evaluate conss) conss t))])
       (unless inst/t
         (raise (type-check-failed "not Id-type" (current-continuation-marks)
                                   cxt e t '(? =? ?))))
       ((construct-type-check conss) conss
        cxt (dict-ref inst/e 'a) (dict-ref inst/t 'A))
       (let ([a/nf ((construct-evaluate conss) conss (dict-ref inst/e 'a))]
             [x/nf ((construct-evaluate conss) conss (dict-ref inst/t 'x))]
             [y/nf ((construct-evaluate conss) conss (dict-ref inst/t 'y))])
          (unless (and (stxequal a/nf x/nf) (stxequal a/nf y/nf))
            (raise (type-check-failed "type mismatch (index)" (current-continuation-marks)
                                      cxt e t ((mkstx-build stx/Id)
                                               (hasheq 'A (dict-ref inst/t 'A)
                                                       'x a/nf 'y a/nf))))))))
   ; evaluate
   (make-cong-evaluator stx/id/refl)
   ))

(define stx/id/J
  (make-syntax '(x y p z) '(r A C c) '((C . (x y p)) (c . (z)))
   '(ind r ((p : x = y) => C) ((refl z) => c))))

(define id/J
  (construct "J"
   ; syntax
   stx/id/J
   ; type-infer
   (λ (conss cxt e)
     (let ([inst/e ((mkstx-instantiate stx/id/J) e)])
       (let ([x (dict-ref inst/e 'x)]
             [y (dict-ref inst/e 'y)]
             [z (dict-ref inst/e 'z)])
         (define (make-C x* y* p*)
           (stxsubst
            (stxsubst
             (stxsubst (dict-ref inst/e 'C)
              x* x)
             y* y)
            p* (dict-ref inst/e 'p)))
         (let* ([t/r ((construct-type-infer conss) conss cxt (dict-ref inst/e 'r))]
                [inst/t/r ((mkstx-instantiate stx/Id)
                           ((construct-evaluate conss) conss t/r))])
           (unless inst/t/r
             (raise (type-check-failed "not Id-type" (current-continuation-marks)
                                       cxt (dict-ref inst/e 'r) t/r '(? = ? ?))))
           (let ([A (dict-ref inst/t/r 'A)]
                 [a (dict-ref inst/t/r 'x)]
                 [b (dict-ref inst/t/r 'y)])
             ((construct-type-check conss) conss
              (dict-set* cxt
               x A
               y A
               (dict-ref inst/e 'p) ((mkstx-build stx/Id)
                                     (hasheq 'A A 'x x 'y y)))
              (dict-ref inst/e 'C)
              'U)
             ((construct-type-check conss) conss
              (dict-set cxt (dict-ref inst/e 'z) A)
              (dict-ref inst/e 'c)
              (make-C z z ((mkstx-build stx/id/refl) (hasheq 'a z))))
             (make-C a b (dict-ref inst/e 'r)))))))
   ; type-check
   bidir-inferable
   ; evaluate
   (let ([cong-evaluator (make-cong-evaluator stx/id/J)])
     (λ (conss e)
       (let* ([eJ/nf (cong-evaluator conss e)]
              [inst/e ((mkstx-instantiate stx/id/J) eJ/nf)]
              [inst/r ((mkstx-instantiate stx/id/refl) (dict-ref inst/e 'r))])
         (if inst/r
           (let ([x (dict-ref inst/e 'x)]
                 [y (dict-ref inst/e 'y)]
                 [a (dict-ref inst/r 'a)])
             (unless (and (stxequal x a) (stxequal y a))
               (raise (eval-error "not .a .a (refl a)" (current-continuation-marks)
                                  eJ/nf)))
             ((construct-evaluate conss) conss
              (stxsubst (dict-ref inst/e 'c) a (dict-ref inst/e 'z))))
           eJ/nf))))
   ))

(define all-constructs
  (list
   anno
   let/subst

   type/U

   type/Sigma
   pair/cons
   pair/ind

   type/coproduct
   coproduct/inl
   coproduct/inr
   coproduct/ind

   type/Id
   id/refl
   id/J

   type/->
   type/Pi
   fn/abs
   fn/ap))

(define var? symbol?)

(define (case-construct constructs-list e)
  (for/or ([variant constructs-list])
    (let ([is-instance? (mkstx-instanceof (construct-syntax variant))])
      (if (is-instance? e) variant #f))))

(define all-conss
  (construct "MLTT"
   ; syntax
   (mkstx
    '() '() '()
    ; instanceof
    (λ (e) (error 'all-stxs/instanceof "with ~a" e)) ; TODO: better exception
    ; instantiate
    (λ (e) (error 'all-stxs/instantiate "with ~a" e)) ; TODO: better exception
    ; build
    (λ (inst) (error 'all-stxs/build "with ~a" inst)) ; TODO: better exception
    ; check
    (λ (stx cxt e)
      (let ([variant (case-construct all-constructs e)])
        (if variant
         ((mkstx-check (construct-syntax variant))
          stx cxt e)
         (and (var? e) (set-member? cxt e)))))
    ; subst
    (λ (stx subst e)
      (let ([variant (case-construct all-constructs e)])
        (if variant
         ((mkstx-subst (construct-syntax variant))
           stx subst e)
         (dict-ref subst e (λ () e)))))
    ; equal
    (λ (stx subst1 e1 subst2 e2)
      (let ([variant (case-construct all-constructs e1)])
        (or
         (and variant
          ((mkstx-instanceof (construct-syntax variant)) e2)
          ((mkstx-equal (construct-syntax variant))
           stx subst1 e1 subst2 e2))
         (and (var? e1) (var? e2)
          (eq? (dict-ref subst1 e1 (λ () e1))
              (dict-ref subst2 e2 (λ () e2))))))))
   ; type-infer
   (λ (conss cxt e)
;     (display (format "typeinfer ~a |-\n~a => ?\n\n" cxt e))
     (let ([variant (case-construct all-constructs e)])
       (cond [variant ((construct-type-infer variant) conss cxt e)]
             [(var? e) (dict-ref cxt e)]
             [else (raise (type-infer-failed "unknown syntax" (current-continuation-marks)
                                             cxt e))])))
   ; type-check
   (λ (conss cxt e t)
;     (display (format "typecheck ~a |-\n~a <= ~a\n\n" cxt e t))
     (let ([variant (case-construct all-constructs e)])
       (cond [variant ((construct-type-check variant) conss cxt e t)]
             [(var? e) (bidir-inferable conss cxt e t)]
             [else (raise (type-check-failed "unknown syntax" (current-continuation-marks)
                                             cxt e t '?))])))
   ; evaluate
   (λ (conss e)
;     (display (format "evaluate ~a = ?\n" e))
     (let ([variant (case-construct all-constructs e)])
       (cond [variant ((construct-evaluate variant) conss e)]
             [(var? e) e]
             [else (raise (eval-error "unknown syntax" (current-continuation-marks)
                                      e))])))
   ))

(define all-stxs (construct-syntax all-conss))

; e1 [e2 / x]
(define (stxsubst e1 e2 x) ((mkstx-subst all-stxs) all-stxs (hasheq x e2) e1))
(define (stxcheck e) ((mkstx-check all-stxs) all-stxs (seteq) e))
(define (stxequal e1 e2) ((mkstx-equal all-stxs) all-stxs (hasheq) e1 (hasheq) e2))

(define (test)
  (with-handlers
    ([type-infer-failed? (λ (t) (display (format "\n~a\n\n" t)))]
     [type-check-failed? (λ (t) (display (format "\n~a\n\n" t)))]
     [eval-error? (λ (t) (display (format "\n~a\n\n" t)))])
#|
    ((construct-type-infer all-conss) all-conss
     (hasheq)
     '
     (((λ f (f (f U))) : ((U → U) → U)) (λ t (t → t)))
     )
;|#
#|
    ((construct-evaluate all-conss) all-conss
     '(((λ f (f (f U))) : ((U → U) → U)) (λ t (t → t)))
     )
;|#
#|
    ((construct-type-infer all-conss) all-conss
     (hasheq)
     '(Π (A : U) (A → A))
     )
;|#
#|
    ((construct-type-infer all-conss) all-conss
     (hasheq)
     '((λ _ (λ x x)) : (Π (A : U) (A → A)))
     )
;|#
#|
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U   'B 'U   'C '(A → (B → U)))
     '((λ f (λ y (λ x ((f x) y)))) :
       ((Π(x : A) (Π(y : B) ((C x) y))) → (Π(u : B) (Π(v : A) ((C v) u)))))
     )
;|#
#|
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'B '(A → U) 'C '((Σ(w : A) (B w)) → U))
     '((λ f (λ pr ((f (fst pr)) (snd pr)))) :
       ((Π(a : A) (Π(b : (B a)) (C (a |,| b)))) → (Π(q : (Σ(x : A) (B x))) (C q))))
     )
;|#
#|
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'B '(A → U) 'C '(A → U))
     '((λ f (λ a (λ b (f (a |,| b))))) :
       ((Π(q : (Σ(w : A) (B w))) (C (fst q))) → (Π(z : A) ((B z) → (C z)))))
     )
;|#
#|
    ; symmetric
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'x 'A 'y 'A)
     '((λ r (ind r ((p : a = b) => (b = A a)) ((refl z) => (refl z)))) :
       ((x = A y) → (y = A x)))
     )
;|#
#|
    ; transitive
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'x 'A 'y 'A 'z 'A)
     '((λ x=y
         (ind x=y ((p : a = b) => ((b = A z) → (a = A z))) ((refl w) => (λ r r)))) :
       ((x = A y) → ((y = A z) → (x = A z))))
     )
;|#
#|
    ; ap (cong)
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'B 'U 'f '(A → B) 'x 'A 'y 'A)
     '((λ x=y
         (ind x=y ((p : a = b) => ((f a) = B (f b))) ((refl z) => (refl (f z))))) :
       ((x = A y) → ((f x) = B (f y))))
     )
;|#
#|
    ; transport (subst)
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'P '(A → U) 'x 'A 'y 'A)
     '((λ x=y
         (ind x=y ((p : a = b) => ((P a) → (P b))) ((refl z) => (λ px px)))) :
       ((x = A y) → ((P x) → (P y))))
     )
;|#
#|
    ((construct-evaluate all-conss) all-conss
     ((construct-type-infer all-conss) all-conss
      (hasheq 'A 'U 'v 'A)
      '(ind ((refl v) : ((((λ u u) : (A → A)) v) = A v))
            ((p : a = b) => (p = (a = A b) p))
            ((refl z) => (refl (refl z))))
      ))
;|#
#|
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'x 'A 'y 'A)
     '((refl (x |,| y)) :
       ((x |,| y) = (Σ(u : A) A) ((((λ u u) : (A → A)) x) |,| y)))
     )
;|#
#|
    ; snd : (p : Σ A B) → B (fst p)
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'B '(A → U) 'p '(Σ(x : A)(B x)))
     '(ind p (z => (B (ind z (_ => A) ((a |,| b) => a))))
             ((x |,| y) => y))
     )
;|#
#|
    ; without induction; type error
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'B '(A → U) 'p '(Σ(x : A)(B x)))
     '((refl p) :
       (p = (Σ(x : A)(B x))
          ((ind p (z => A) ((x |,| y) => x)) |,|
           (ind p (z => (B (ind z (_ => A) ((x |,| y) => x))))
                  ((x |,| y) => y)))))
     )
;|#
#|
    ; with induction; okay
    ((construct-type-infer all-conss) all-conss
     (hasheq 'A 'U 'B '(A → U) 'q '(Σ(x : A)(B x)))
     '(ind q
       (p => (p
              = (Σ(x : A)(B x))
              ((ind p (z => A) ((x |,| y) => x)) |,|
               (ind p (z => (B (ind z (_ => A) ((x |,| y) => x))))
                      ((x |,| y) => y)))))
       ((a |,| b) => (refl (a |,| b))))
     )
;|#
#|
    ((construct-evaluate all-conss) all-conss
     ((construct-type-infer all-conss) all-conss
      (hasheq 'A 'U 'B '(A → U) 'q '(Σ(x : A)(B x)))
      '(let fst : ((Σ(x : A) (B x)) → A) =
         (λ p (ind p (z => A) ((x |,| y) => x)))

         (let snd : (Π(p : (Σ(x : A) (B x))) (B (fst p))) =
          (λ p (ind p (z => (B (fst z))) ((x |,| y) => y)))

          (ind q (p => (p = (Σ(x : A)(B x)) ((fst p) |,| (snd p))))
                ((a |,| b) => (refl (a |,| b))))))))
;|#
#|
    ((construct-evaluate all-conss) all-conss
     ((construct-type-infer all-conss) all-conss
      (hasheq 'A 'U 'B 'U 'a 'A 'b 'B 'C 'U)
      '(let x : (A :+: B) = (inl a)
        (let y : (A :+: B) = (inr b)
         (let f : ((A :+: B) → U) =
           (λ z (ind z (w => U)
                ((inl i) => A)
                ((inr j) => B)))
          (ind x (z => (f z))
               ((inl u) => u)
               ((inr v) => v)))))))
;|#
;#|
    ((construct-evaluate all-conss) all-conss
     '(let x : (A :+: B) = (inl a)
       (let y : (A :+: B) = (inr b)
        (ind y (z => (ind z (w => U)
                            ((inl i) => (A :+: A))
                            ((inr j) => (B :+: B))))
             ((inl u) => (inr u))
             ((inr v) => (inl v))))))
;|#
    ))
