#lang racket

(require (for-syntax syntax/parse))

(provide (all-defined-out))

(define-syntax a-first-macro
  (λ (stx)
    (printf "a-first-macro called with value: ~a\n"
            (syntax->datum stx))
    (syntax #t)))

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

(define-syntax scanf-define
  (λ (stx)
    (syntax-parse stx
      [(_ fmt:string x:id ...)
       (define fmt-str (syntax-e (syntax fmt)))
       (define expected-length (count-format-string fmt-str (syntax fmt)))
       (define given-length (length (syntax->list (syntax (x ...)))))
       (unless (equal? expected-length given-length)
         (raise-syntax-error 'scanf-define-good
                             (format "expected ~a variables but got ~a"
                                     expected-length given-length)
                             stx))
       (syntax
        (define-values (x ...)
          (apply values
            (read-format fmt))))]
      [(_ fmt x:id ...)
       (syntax
        (define-values (x ...)
          (apply values
            (read-format fmt))))])))

(module+ main
  (a-first-macro 1 x "hello" #t)

  a-first-macro

  (when #f
    (define-syntax (scanf-define stx)
      (syntax-parse stx
        [(_ fmt x:id ...)
         (syntax
          (define-values (x ...)
            (apply values
              (read-format fmt))))]))
    (scanf-define "%d%d\n" a b)
    (printf "read: ~a ~a\n" a b))

  (scanf-define "%d%d\n" x y)
  (printf "point: x = ~a, y = ~a\n" x y))
