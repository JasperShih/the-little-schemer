#lang racket

(define eternity
  (lambda (x)
    (eternity x)))

(define add1
  (lambda (n)
    (+ n 1)))

(define length
  (lambda (lst)
    (cond [(null? lst) 0]
          [else (add1 (length (cdr lst)))]
          )))


(
(
(lambda (mk)
  (mk mk))
(lambda (le)
   (lambda (lst)
     (cond [(null? lst) 0]
           [else (add1 ((le le) (cdr lst)))]
           )))
)
'())





(
(lambda (mk)
  (mk mk))

(lambda (mk)
   (lambda (lst)
     (cond [(null? lst) 0]
           [else (add1 ((mk mk) (cdr lst)))]
           )))
)



(
(lambda (mk)
  (mk mk))

(lambda (mk)
  (lambda (length)
    ((lambda (lst)
       (cond [(null? lst) 0]
             [else (add1 (length (cdr lst)))]
             )))
    (mk mk)
    )
  )
)




((lambda (le)
   ((lambda (mk)
      (mk mk))  
   (lambda (mk)
     (le (lambda (x) ((mk mk) x))))))
 
 (lambda (length)
   (lambda (lst)
      (cond [(null? lst) 0]
             [else (add1 (length (cdr lst)))]
             )))
 )




(
 (lambda (mk)
   ((lambda (length)
      (lambda (lst)
        (cond [(null? lst) 0]
              [else (add1 (length (cdr lst)))]
              )))
    (mk mk))
   )

 (lambda (mk)
   (lambda (lst)
     (cond [(null? lst) 0]
           [else (add1 ((mk mk) (cdr lst)))]
           )))
 )




;(define length
 ; (lambda (lst)
  ;  (cond [(null? lst) 0]
   ;       [else (add1 (length (cdr lst)))]
    ;      )))



























