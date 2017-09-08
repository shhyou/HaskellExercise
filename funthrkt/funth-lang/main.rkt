#lang racket

; 實作自己的語言 #%module-begin, #%app, #datum 等 "hook"
(provide
 + - * / add1
 (rename-out
  [new-module-begin #%module-begin]
  [new-app #%app]
  [new-datum #%datum])
 #%top-interaction)

(require (for-syntax syntax/parse syntax/srcloc))

; 這個語言的 reader 模組, 用 `syntax/module-reader` 語言寫成.
; 還有一個有趣的是像 `s-exp`, `at-exp` 等 meta language. 關於
; meta-language 可以看 `make-meta-reader` API.
(module reader syntax/module-reader
  funth-lang)


; module begin 是作用在整個模組上的 transformer. 在這裡我們
; 直接限定整個模組只能有恰好一個 expression. 如果實作 typed
; language 的話, 可以在這裡把所有 form 都展開後做 type checking
; 推薦讀 Languages as libraries 這篇文章
(define-syntax new-module-begin
  (λ (stx)
    (syntax-parse stx
      [(_ body:expr)
       (syntax
        (#%module-begin
         (printf "Evaluation the expression ~a\n" 'body)
         body))]
      [_ (raise-syntax-error 'funth-lang "body must be exactly one expression" stx)])))


; 實作自己的 function application. 這裡我們印出整個 application
; 的過程. 如果實作 lazy language, 也需要寫自己的 function application
(define-syntax new-app
  (λ (stx)
    (syntax-parse stx
      [(_ e ...)
       (quasisyntax
        (logging-app
         '(unsyntax (source-location->string stx))
         '(e ...)
         (list (λ () e) ...)))])))


; 實作這個語言在資料上的 hook. 這裡我們限制語言中寫的資料只能是
; 整數. 在展開過程中, 所有的資料都會先過 #%datum macro. 預設的
; #%datum 會把資料 quote 起來
; 關於 Racket expander 怎麼插入 #%app 跟 #%datum 的演算法可以參考
; Racket Reference 1.2.3.2, Expansion Steps
(define-syntax new-datum
  (λ (stx)
    (syntax-parse stx
      [(_ . d:integer)
       (quasisyntax
        (logging-datum
         '(unsyntax (source-location->string stx))
         'd))]
      [_ (raise-syntax-error 'new-datum "only integer datum allowed" stx)])))


(define logging-app
  (λ (location expr thunks)
    (match-define `(,f ,args ...)
      (for/list ([e_i (in-list thunks)])
        (e_i)))
    (define result
      (apply f args))
    (printf "\n~a\n    ~a\n;=> ~a\n;=> ~a\n"
            (~a #:max-width 25 #:limit-prefix? #t #:limit-marker "..." location)
            expr (cons f args) result)
    result))


(define logging-datum
  (λ (location dat)
    (printf "\n~a\n    datum: ~a\n"
            (~a #:max-width 25 #:limit-prefix? #t #:limit-marker "..." location)
            dat)
    dat))
