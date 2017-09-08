#lang racket

(require (for-syntax syntax/parse racket/pretty racket/base racket/string)
         syntax/parse)

(define-syntax 我的第一個元編程transformer
  (λ (stx)
    (pretty-print (syntax-e stx))
    (syntax #true)))

(我的第一個元編程transformer 1 2 3 + "abcd")

;(+ 1 我的第一個元編程transformer)

(define-syntax 2nd-transformer
  (λ (stx)
    (syntax-parse stx
      [(_ x y z)
       (printf "x = ~a\ny = ~a\n" (syntax x) (syntax y))
       (syntax (* x y))])))

(2nd-transformer 1 2 "3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define read-int
  (λ ()
    (define data
      (regexp-try-match #px"^\\s*\\d+(\\.\\d+)?" (current-input-port)))
    (if data
        (string->number (string-trim (bytes->string/utf-8 (first data))))
        (error 'read-int "not a number"))))

(define read-string
  (λ ()
    (define data
      (regexp-try-match #px"^\\s*[^\\s]+" (current-input-port)))
    (if data
        (string-trim (bytes->string/utf-8 (first data)))
        (error 'read-string "not a string"))))

(define read-format
  (λ (fmt)
    (define (read-expected-char expected-ch)
      (define ch (read-char))
      (unless (equal? ch expected-ch)
        (error 'read-format "read does not match")))
    (define (get-read-proc i)
      (define ch (string-ref fmt i))
      (cond
        [(equal? #\d ch) (values (λ (k) (cons (read-int) (k))) 1)]
        [(equal? #\s ch) (values (λ (k) (cons (read-string) (k))) 1)]
        [(equal? #\% ch) (values (λ (k) (read-expected-char #\%) (k)) 1)]
        [else (error 'read-format "unknown format string")]))
    (let loop ([i 0])
      (cond
        [(< i (string-length fmt))
         (cond
           [(and (equal? #\% (string-ref fmt i))
                 (< (+ i 1) (string-length fmt)))
            (define-values (do-read step) (get-read-proc (+ i 1)))
            (do-read (λ () (loop (+ i step 1))))]
           [else
            (read-expected-char (string-ref fmt i))
            (loop (+ i 1))])]
        [else '()]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define return-two-values
  (λ (x)
    (values (* x 2) (* x x))))

(define-values (a b)
  (return-two-values 9))

(call-with-values
 (λ () (return-two-values 5))
 (λ args args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define count-format-string
    (λ (fmt stx)
      (let loop ([i 0])
        (cond
          [(< i (string-length fmt))
           (cond
             [(and (equal? #\% (string-ref fmt i))
                   (< (+ i 1) (string-length fmt)))
              (define ch (string-ref fmt (+ i 1)))
              (cond [(equal? #\d ch) (+ 1 (loop (+ i 2)))]
                    [(equal? #\s ch) (+ 1 (loop (+ i 2)))]
                    [(equal? #\% ch) (loop (+ i 2))]
                    [else (raise-syntax-error 'scanf-define-good
                                              "format string incorrect"
                                              stx)])]
             [else (loop (+ i 1))])]
          [else 0])))))

(define-syntax testxx
  (λ (stx)
    (syntax
     (syntax-parse (syntax #f)
       [_ "what"]))))

(testxx 1 2 3)

; (my-scanf 第一個參數  a ( c d e ) g x y z)
;   fmt  -----^
;    x ...            ^~~~~~~~~~~~~~~~~~~~
; (syntax (u v w x ... e f g))
; ===>    (u v w   a ( c d e ) g x y z   e f g)
(define-syntax my-scanf
  (λ (stx)
    (syntax-parse stx
      [(_ fmt:string x ...)
       ; (x ...)
       (define fmt-data
         (syntax-e (syntax fmt)))
       (define len
         (length (syntax->list (syntax (x ...)))))
       (unless (equal? len
                       (count-format-string fmt-data (syntax fmt)))
         (raise-syntax-error 'my-scanf "wrong argument count" stx))
       (syntax
        (define-values (x ...)
          (apply values (read-format fmt))))])))

(my-scanf "%d%d" u v)

(vector u)

(struct my-point [x-coord y-coord] #:transparent)

(define a-pt (my-point 5 3))
(my-point? a-pt)
(my-point-x-coord a-pt)
