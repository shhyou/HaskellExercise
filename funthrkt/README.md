Functional Thursday #55 程式碼
=====

- `on-the-fly.rkt`: 現場討論跟寫的程式碼. 大部分是 `intro.rkt` 的內容

- `intro.rkt`: 介紹 Racket 中 metaprogramming 最基本的構造, `define-syntax`
               以及 compilation-time (＊) 執行任意程式來檢查參數格式

- `my-lang/`, `hello-sexp.rkt`, `hello-lang.rkt`:
    "API" 示範 I, 示範 Racket 程式最上面那行 `#lang` 的 protocol 可以怎麼實作.
    Racket 最基礎的設計以一個檔案為一個模組, 每個檔案最一開始的 `#lang` 後面
    指定該檔案(模組)使用的語言. 對一個模組來說, 其使用的語言會提供 parser 讓
    Racket 能夠 parse 該模組檔案的內容, 而且檔案中的內容最一開始也都會 bind
    到語言提供的 binding. 每個模組在 parse 後事實上會長這樣

    ```racket
    (module ID LANG-MODULE
      (#%module-begin
        form ...))
    ```

    (`#%module-begin` 需要由 `LANG-MODULE` 提供, 這裡也讓 `LANG-MODULE` 有了
    對整個模組變換的自由. 以 `#lang racket` 提供的 `#%module-begin` 為例,
    他會把所有最上層的 expression 都包在 print 裡面. 這是為什麼當我們寫

    ```racket
    #lang racket
    1 #t "hello world"
    ```

    會印出

    ```racket
    1
    #t
    "hello world"
    ```

    的原因. 另外兩個相關檔案 `hello-sexp.rkt` 跟 `hello-lang.rkt` 用了兩種
    不同的指定 `LANG-MODULE` 的方法

    - `hello-sexp.rkt` 用的是最懶人的方式, 透過 `s-exp` 這個 meta-language
         直接在後面指定實作語言的模組

    - `hello-lang.rkt` 使用前需要先安裝 package, 讓 `funth-lang` 指到語言
         的實作. 這也是最常見的情況. 要安裝 package 可以直接在 `funth-lang`
         中執行 `raco pkg install`:

         ```
         $ cd funth-lang
         funth-lang/ $ ls
         info.rkt  main.rkt
         funth-lang/ $ raco pkg install
         ```

         要移除這個 package 的話執行:

         ```
         raco pkg remove funth-lang
         ```

- `dynrun.rkt`: 演示如何動態執行一段程式碼或載入一個模組. `run-dynamic-require`
                設定相關的環境(namespace 跟 modules)後用 `dynamic-require` 來動態
                的載入一個模組, 而 `read-module` 和 `eval-module` 則手動讀取
                檔案後, 再用 `eval` 和 `dynamic-require` 來 evaluate 和 instantiate
                讀取的模組.

    ```racket
    $ racket
    Welcome to Racket v6.10.0.3.
    > (require "dynrun.rkt")
    > (define read-int (run-dynamic-require "intro.rkt" 'read-int))
    a-first-macro called with value: (a-first-macro 1 x hello #t)
    a-first-macro called with value: a-first-macro
    > (define n (read-int))
    8
    > (* n n)
    64
    > (define m (read-module "hello-sexp.rkt"))
    > m
    #<syntax:1:0 (module hello-sexp "funth-lan...>
    > (eval-module m ''hello-sexp)
    (略)
    ```

    `dynamic-require` 把 `eval` 封裝成比較好用的 API. Racket 中非常多
    特性(如 phase separation, module languages, 還有比較好的 definition
    context 等)在模組的層面才能有比較完善的定義跟支援, 所以專門針對
    動態載入(visit / instantiate)模組的 `dynamic-require` 通常比較好用.

    另一方面, `eval` 雖然不可或缺, 但也是比較底層的 API. 這我們可以在
    `hooks.rkt` 中看到. 在最底層動態計算一個算式, 或 Racket 載入模組,
    使用者在 REPL 中的操作等還是會用到 `eval`. 但同時 `eval` 所在的
    top-level [也是沒救的](https://gist.github.com/takikawa/3941612).
    舉例來說, 在模組的層次, 要定義 mutual-recursive function **是什麼意思**
    很容易:

    ```racket
    #lang racket
    (define (f x) (g x))
    (define (g x) (f x))
    ```

    這是因為我們有整個模組完整的程式碼, 知道這個模組總共有兩個定義, `f`
    跟 `g`. 所以在考慮 `f` 跟 `g` 的程式碼中 `g` 和 `f` 分別指什麼意思時,
    我們能知道 `g` 跟 `f` 指的就是這個模組中的 `g` 跟 `f` 兩個定義.
    但是在 top-level (還有 `eval`)中, 這兩個定義會是分開被 evaluate 的:

    ```racket
    $ racket
    Welcome to Racket v6.10.0.3.
    > (define (f x)   (g x))
    > (f 5)
    ; g: undefined;
    ;  cannot reference undefined identifier
    ; [,bt for context]
    > (define (g x) (f x))
    > (f 3)
    ^C; user break [,bt for context]
    > (define (g x) #f)
    > (f 9)
    #f
    ```

    也就是說, 當我們在 evaluate `f` 的時候, `g` 是個 unbound identifier.
    在這種狀況下, top-level variable 需要特殊處理. 我們沒辦法靜態的知道
    top-level variable 到底 refer 到 "哪一個" 定義. 特別的, top-level
    variable 跟 macro 的互動相當不好. 通常我們會直接在模組的層次寫程式
    而不是在 top-level 環境. 以模組為單位寫程式的話, 即使動態的產生程式碼並用
    `eval` 跟 `dynamic-require` 也不會讓人覺得意義不明.

- `hooks.rkt`: 這個程式試圖 demo Racket 執行一個程式的全程. 從解析模組的
    名字開始, 一路到從檔案系統載入模組(或載入 compiled byte code),
    載入其 dependencies, 到最後編譯並執行模組然後註冊模組名字
    為止, 這個程式在許多的 API 都會印出訊息.
    [第 95 行](https://github.com/shhyou/HaskellExercise/blob/master/funthrkt/hooks.rkt#L95)
    是插入印東西的函式的地方.

    以 `current-compile` 為例, 這個參數控制了 Racket 當前用的 compiler
    是誰. 像 [errortrace](https://docs.racket-lang.org/errortrace/using-errortrace.html)
    等函式庫就會插入自己的 compiler 來對被編譯的 module 做標記, 插入追蹤
    code 等等.

    在測試中, 可以觀察 `funth-lang/` 下的程式有沒有編譯對執行
    `hello.rkt` 有什麼影響:

    ```
    $ cd funth-lang/
    funth-lang/ $ raco pkg install
    funth-lang/ $ cd ..
    $ racket hooks.rkt
    ... [blah] ...
    $ rm -rf funth-lang/compiled/
    $ racket hooks.rkt
    ... [blah] ...
    $ raco pkg remove funth-lang
    ```

(＊): 準確的說是 expansion-time
