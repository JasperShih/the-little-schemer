#lang racket


(define interp
  (lambda (exp)
    (cond [(atom? exp) (cond [(number? exp) exp]
                             [else (define exps (list [list '+ +]
                                   			          [list '- -]
                                          			  [list '* *]
                                                      [list '/ /]))
                 	   			   (engage exp exps 'not_found)
                              ])
           ]
          [else (define funct (interp (car exp)))
           		(define arg_1 (interp (cadr exp)))
             	(define arg_2 (interp (caddr exp)))
                (funct arg_1 arg_2)
             ]
          )))


;;一個參數與多參數
;;自動補全與自動刪除雙括號引號
;;autoformating, indent

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


(interp '(+ 1 2 3 4)
        )






