#lang racket

(define interpreter
  (lambda (exp)
    
    [define handle_car car]
    [define handle_cdr cdr]
    [define handle_cons
     (lambda (arg_1)
       (lambda (arg_2)
         (cons arg_1 arg_2)))]
    [define handle_zero zero?]
    
    ; 若直接(cadr exp), 如果exp是atom會出錯, 改成函數形式, call才求值
    [define handle_quote
     (lambda ()
       (cadr exp))] 
    
    [define handle_app
     (lambda ()
       (define funct (interpreter (car exp)))
       (define apply_args
         (lambda (args)
           (cond [(null? args) funct]
                 [else (set! funct (funct (interpreter (car args))))
                       (apply_args (cdr args))]
                 )))
       (apply_args (cdr exp))
        )]
    
    [define atom_exp_flow
      (lambda ()
        (cond [(number? exp) exp]
              [else 
			        (define atoms (list [list 'car handle_car]
			                       		[list 'cdr handle_cdr]
			                         	[list 'cons handle_cons]
			                          	[list 'zero? handle_zero]))
			        (engage exp atoms 'not_found)]
              )
       )]
    
    [define list_exp_flow
      (lambda ()
        (define lists (list [list 'zero? (handle_app)]
                            [list 'quote 'handle_quote]))
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


(interpreter '(zero? 10))





;;開發週期:衝刺, 思考. 一直repeat
;;all_in_one function的可讀性高?, 開發快, 程式碼少;
;;缺少模組化, 應該先all_in_one寫出來後再refactor?