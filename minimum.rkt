#lang racket

(define currying_2
  (lambda (funct)
    (lambda (arg_1)
      (lambda (arg_2)
        (funct arg_1 arg_2)))))

(define handle_add
  (currying_2 +))

(define handle_subtract
  (currying_2 -))

(define handle_multiply
  (currying_2 *))

(define handle_divide
  (currying_2 /))

(define interp
  (lambda (exp)
    (cond [(atom? exp) (cond [(number? exp) exp]
                             [else (define exps (list [list '+ handle_add]
                                                      [list '- handle_subtract]
                                                      [list '* handle_multiply]
                                                      [list '/ handle_divide]
                                                      ))
                                   (engage exp exps 'not_found)
                                   ])
                       ]
          
          
          [else (define funct (interp (car exp)))
                (define apply_args
                  (lambda (args)
                    (cond [(null? args) funct]
                          [else (set! funct (funct (interp (car args))))
                                (apply_args (cdr args))]
                          )))
                (apply_args (cdr exp))
                ]
          )))


;;先假定沒有無限參數好了, 加減乘除就只有二元運算
;;自動補全與自動刪除雙括號引號

(define engage
  (lambda (exp candidates fail)
    (cond [(null? candidates) fail]
          [(eq? exp (caar candidates)) (cadar candidates)]
          [else (engage exp (cdr candidates) fail)]
          )))

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x))
         )))


(interp '(* (- (+ 2 5) (/ 8 2)) 6)
        )

;(interp '(- 2 5))

;(length '(1 2 3 8))


;;What is list?
;;List is function!
;;What is data structure???
;;Data structrue is function!!!
;;What is object?
;;Object is function!

;;什麼是quote? quote就是不要求值的意思





