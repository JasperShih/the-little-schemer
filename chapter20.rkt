#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; what is function? function is that you give args to it, it will return values back.
;;
;; what is table? table is that you give a name to it, it will search the same name
;; in its data structure then return the value of name.
;;
;; so table can be represented by function, we give table a name arg, then it return a value

(define global_table '())

(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name value table)
    (lambda (target)
      (cond [(eq? target name) value]
            [else (table target)]
            ))
    ))

(define define?
  (lambda (exp)
    (cond [(atom? exp) #f]
          [(atom? (car exp)) (eq? (car exp) 'define)]
          [else #f]
          )))

(define name_of
  (lambda (exp)
    (cadr exp)))

(define get_right_side
  (lambda (exp)
    (cddr exp)))

(define handle_define
  (lambda (exp)
    (set! global_table (extend (name_of exp)
                               (box (the_meaning (get_right_side exp)))
                               global_table))))

(define box
  (lambda (original_value)
    (lambda (selector)
      (selector original_value
                (lambda (new_value) (set! original_value new_value)))
      )))

;; functional programming 的function為動詞, 其他為名詞; 我認為應該分清楚避免混淆概念

(define setbox
  (lambda (box new_value)
    (box (lambda (original_value update) (update new_value)))))

(define get_value
  (lambda (box)
    (box (lambda (original_value update) original_value))))


(define the_meaning
  (lambda (exp)
    (meaning exp lookup_in_global_table)))

(define lookup_in_global_table
  (lambda (name)
    (lookup global_table name)))

(define meaning
  (lambda (exp table)
    ((exp_selector exp) exp table)))

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





























