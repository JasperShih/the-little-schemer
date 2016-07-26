#lang racket


;; what is function? function is that you give args to it, it will return values back.
;;
;; what is table? table is that you give a name to it, it will search the same name
;; in its data structure, then return the value of name.
;;
;; so table can be represented by function, we give table a name arg, then it return a value


(define empty_search_table
  (lambda (target_name)
    (abort ((cons target_name 
                  '(is no anwser)))
      )))

(define search_global_table empty_search_table)

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x))
     )))


(define extend_search_table
  (lambda (inserted_entry_name
           inserted_entry_value
           original_search_table)  
    (letrec ([new_search_table (lambda (search_name)
                                 (cond [(eq? search_name 
                                             inserted_entry_name) inserted_entry_value]
                                       [else (original_search_table search_name)]))]
             )
    new_search_table
    )))


;; 新增規則, 動詞或後綴func都是function?
(define pack
  (lambda (value)
    (letrec ([update_value (lambda (new_value) (set! value new_value))]
             [func_package (lambda (pick) (pick value update_value))]
             )
      func_package
      )))

=======

(define setbox
  (lambda (box new_value)
    (box (lambda (original_value update) (update new_value)))))

(define get_value
  (lambda (box)
    (box (lambda (original_value update) original_value))))


(define the_meaning
  (lambda (exp)
    (meaning exp lookup_in_search_global_table)))

(define lookup_in_search_global_table
  (lambda (name)
    (search_global_table name)))

(define meaning
  (lambda (exp table)
    ((exp_selector exp) exp table)))


(define handle_define
  (lambda (exp)
    (set! search_global_table (extend_search_table (cadr exp)
                               (box (the_meaning (caddr exp)))
                               search_global_table)
          )))


(define handle_quote
  (lambda (exp table)
    exp))

(define handle_identifier
  (lambda (exp table)
    (unbox (lookup table exp))))

(define handle_set!
  (lambda (exp table)
    (setbox (lookup table (cadr exp)) (meaning (caddr exp) table)
     )))

(define handle_lambda
  (lambda (exp table)
    (lambda (args)
      (deal_lambda_exps (cddr exp)
                        (multi_extend (cadr exp)
                                      (box_all args)
                                      table)
                        ))))

(define deal_lambda_exps
  (lambda (exps table)
    (cond [(null? (cdr exps)) (meaning (car epxs) table)]
          [else (meaning (car epxs) table)
                (deal_lambda_exps (cdr exps) table)]
          )))

(define box_all
  (lambda (args)
    (cond [(null? args) '()]
          [else (cons (box (car args)) (box_all (cdr args)))]
          )))
;; box_all 直接box expressions, 而沒有求meaning, 是否到handle_app時才求meaning?

(define multi_extend
  (lambda (names values table)
    (cond [(null? names) table]
          [else (multi_extend (cdr names)
                              (cdr values)
                              (extend (car names)
                                      (car values)
                                      table))]
          )))
;; 和書中相反


(define odd?
  (lambda (n)
    (cond [(zero? n) #f]
          [else (even? (- n 1))]
          )))

(define even?
  (lambda (n)
    (cond [(zero? n) #t]
          [else (odd? (- n 1))]
          )))

(define handle_app
  (lambda (exp table)
    ((meaning (car exp) table)
     (eval_list (cadr exp) table)
     )))

;;(toy 1 2 3)

(define eval_list
  (lambda (args table)
    (cond [(null? args) '()]
          [else (cons (meaning (car args) table)
                      (eval_list (cdr args) table))]
          )))

(define one_prim
  (lambda (function)
    (lambda (args_list)
      (function (car args_list)))))

(define two_prim
  (lambda (function)
    (lambda (args_list)
      (function (car args_list) (cadr args_list)))))

(define handle_const
  (lambda (:cons :car :cdr :null? :eq? :atom? :zero? :number?)
    (lambda (exp table)
      (cond [(number? exp) exp]
            [(eq? exp #t) #t]
            [(eq? exp #f) #f]
            [(eq? exp 'cons) :cons]
            [(eq? exp 'car) :car]
            [(eq? exp 'cdr) :cdr]
            [(eq? exp 'null?) :null?]
            [(eq? exp 'eq?) :eq?]
            [(eq? exp 'atom?) :atom?]
            [(eq? exp 'zero?) :zero?]
            [(eq? exp 'number?) :number?]
            ))
    (two_prim cons)
    (one_prim car)
    (one_prim cdr)
    (one_prim null?)
    (two_prim eq?)
    (one_prim atom?)
    (one_prim zero?)
    (one_prim number?)
    ))

;; 我們知道我們的interpreter會為app的args extend table, 所以我們不用重複計算args部分?


(define handle_cond
  (lambda (exp table)
    (evcon (cdr exp) table)))

(define evcon
  (lambda (lines table)
    (cond [(eq? (caar lines) 'else) (meaning (cadar exp) table)]
          [(meaning (caar lines) table) (meaning (cadar exp) table)]
          [else (evcon (cdr lines table))]
          )))


(define handle_letcc
  (lambda (exp table)
    (let/cc skip
      (deal_lambda_exps (cddr exp) (extend
                                    (cadr exp)
                                    (box (one_prim skip))
                                    table)))))

;; 在我們使用了客製letcc函數後, 要擔心的只有一個問題, 就是afterward的程式碼中遇到(客製letcc的第一個參數名)要如何處理?
;; 於是我們先用scheme的letcc來interpret客製letcc後, 再來處理afterward的程式碼.
;;
;;
;; 這裡只是用letcc來解釋letcc, 而不是去考慮如何用lambda去實現letcc;
;; 這裡只做兩件事:
;; 1.解釋客製letcc的第一個參數名字(在talbe中加入該參數名, 以及它的值, 使得該參數名有了意義)
;;   他的值其實就是letcc skip(就只是用scheme內建的letcc來實現我們的客製letcc)
;;
;; 2.計算deal_lambda_exps, 如果在計算中遇到該參數名, 1就是該參數的意義, 所以所有的語法我們的interpreter都認得,
;;   就如同以前處理deal_lambda_exps一樣


(define abort '())

(define interpreter
  (lambda (exp)
    (let/cc the_end
      (set! abort the_end)
      (cond [(eq? (car exp) 'define) (handle_define exp)]
            [else (the_meaning exp)]
            ))))

(define exp_to_act
  (lambda (exp)
    (cond [(atom? exp) (atom_to_act exp)]
          [else (list_to_act exp)]
          )))

(define atom_to_act
  (lambda (exp)
    (cond [(number? exp) handle_const]
          [(eq? exp #t) handle_const]
          [(eq? exp #f) handle_const]
          [(eq? exp 'cons) handle_const]
          [(eq? exp 'car) handle_const]
          [(eq? exp 'cdr) handle_const]
          [(eq? exp 'null?) handle_const]
          [(eq? exp 'eq?) handle_const]
          [(eq? exp 'atom?) handle_const]
          [(eq? exp 'zero?) handle_const]
          [(eq? exp 'number?) handle_const]
          [else handle_identifier]
          )))

(define list_to_act
  (lambda (exp)
    (cond [(atom? (car exp)) (cond [(eq? (car exp) 'lambda) handle_lambda]
                                   [(eq? (car exp) 'letcc) handle_letcc]
                                   [(eq? (car exp) 'set!) handle_set]
                                   [(eq? (car exp) 'cond) handle_cond]
                                   [else handle_app]
                                   )]
          [else handle_app]
          )))











