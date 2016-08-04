#lang racket

(define interpreter
  (lambda (exp)
    
    [define handle_car car]
    [define handle_cdr cdr]
    
    ; 若直接(cadr exp), 如果exp是atom會出錯, 改成函數形式, call才求值
    [define handle_quote
     (lambda ()
       (cadr exp))] 
    
    [define handle_app
     (lambda ()
       (interpreter (car exp)) )]
    
    [define atom_exp_flow
      (lambda ()
        (define atoms (list [list 'car handle_car]
                       		[list 'cdr handle_cdr]))
        (engage exp atoms 'not_found)
       )]
    
    [define list_exp_flow
      (lambda ()
        (define lists (list [list 'app handle_app]
                            [list 'quote handle_quote]))
        (engage (car exp) lists 'not_found)
       )]
    
    (cond [(F_atom? exp) (atom_exp_flow)]
          [else (list_exp_flow)]
          )
    ))




(define F_atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x))
     )))


(define engage
  (lambda (exp candidates fail)
    (cond [(null? candidates) fail]
          [(eq? exp (caar candidates)) (cadar candidates)]
          [else (engage exp (cdr candidates) fail)]
          )))


(interpreter '(app aaa))




;;(define atoms '(car cdr))
;;(define handlers (list handle_car handle_cdr))
;;不應用到兩個list

;;應該用[('car handle_car) ('cdr handle_cdr)]及engage



