#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define leave '())

(define walk
  (lambda (lst)
    (cond [(null? lst) '()]
          [(atom? (car lst)) (leave (car lst))]
          [else (let ()
                  (walk (car lst))
                  (walk (cdr lst)))]
          )))

(define start
  (lambda (lst)
    (let/cc here
      (set! leave here)
      (walk lst))))

;(start '(() (A) B C))

(define fill '())

(define wa
  (lambda (lst)
    (cond [(null? lst) '()]
          [(atom? (car lst)) (let ()
                               (let/cc rest
                                 (set! fill rest)
                                 (leave (car lst)))
                               (wa (cdr lst)))]
          [else (let ()
                  (wa (car lst))
                  (wa (cdr lst)))]
          )))

(define start_wa
  (lambda (lst)
    (let/cc here
      (set! leave here)
      (wa lst))))


(define next
  (lambda (x)
    (let/cc here_again
      (set! leave here_again)
      (fill 'go))))

(define get_first
  (lambda (lst)
    (let/cc here
      (set! leave here)
      (wa lst)
      (leave '()))))

(get_first '((D) (C (C (S))) D))

(next 'go)
(next 'go)
(next 'go)
(next 'go)
(next 'go)
(next 'go)
(next 'go)

;; 一個function可能要do a lots of things(我們給這function一個名字叫wa), 這樣wa整個run完可能要花很久的時間以及很多的資源,
;; generator的想法就是就是將break points塞入function中, 將wa分成好幾段, 當我們開始執行這個wa後,
;; 遇到第一個break point時(我們叫他break point A好了), function就停止了(pause), 不再跑了.
;; 然後我們也可以選擇繼續執行(resume)wa, 這時候wa將從break point A開始執行, 直到他遇到第二個
;; break point(我們將他命名為break point B)時又停止了(pause);
;; 值得注意的是在上面的例子中, 當wa執行到break point A而puase後, resume時我們要能夠取得wa在pause當下
;; 的所有資訊(context), 包括wa的args, 變數, data structure 變數, stack, heap, 呼叫或被呼函式的儲存stack,......等等
;; 以上所有的事情, 就只是為了
;; 1)從wa的function entry開始執行到break point A,
;; 2)然後暫停,
;; 3)再從break point A繼續執行(resume)到break point B
;; 要跟[從wa的function entry開始執行, 直到break point B]是一樣的;
;; 也就是說當function [start]然後經過多次[pause&resume]後[finish], 要跟function [start] then [finish] 是一樣的.

;; 我們每call一次generator都將執行function的一小片段, 所以我想它所generate的就是function的片段執行後, 所產生的東西.
;; When every times we call generator, generator will run a phrase, and generate something.

;; generator說穿的就只是pausable function而已, generator這個名字並不符合它的本質, 也不夠直覺, pausable function才夠直接表達它的本質.
;; 舉個例子, 一個影片很長, 假設是10小時, 我們很可能沒那個時間從頭到尾一次看完, 所以我們可能看個兩小時就按下pause,
;; 然後我們就去洗澡,吃飯,做家事,......等等, 等到我們有時間回來看這個影片時, 我們按下play, 影片又開始播放, 這次我們看了3小時後又按下pause,
;; 做做其他事情, 然後又按下play, 一次看五個小時把影片看完.
;; 它明顯是pausable的, 所以我們可以叫它是個generator, 我們每按一次play, 它就將generate一段影片片段給我們.



















