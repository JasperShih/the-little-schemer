#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define first
  (lambda (lst)
    (car lst)))

(define second
  (lambda (lst)
    (cadr lst)))

(define lookup_in_entry
  (lambda (name entry entry_f)
    (lookup_in_entry_help name
    (first entry)
    (second entry)
    entry_f
    )))

(define lookup_in_entry_help
  (lambda (name names values entry_f)
    (cond [(null? names) (entry_f name)]
          [(eq? (car names) name) (car values)]
          [else (lookup_in_entry_help name (cdr names) (cdr values) entry_f)]
          )))

(define extend_table cons)


(define lookup_in_table
  (lambda (name table table_f)
    (cond [(null? table) (table_f name)]
          [else (lookup_in_entry name
                                 (car table)
                                 (lambda (name) (lookup_in_table name
                                                                 (cdr table)
                                                                 table_f)))]
          )))


(define evaluate
  (lambda (expression)
    (react expression '())))

(define react
  (lambda (expression table)
    ((analysis expression) expression table)))

(define analysis
  (lambda (expression)
    (cond [(atom? expression) (atom_analysis expression)]
          [else (list_analysis expression)]
          )))

(define atom_analysis
  (lambda (expression)
    (cond [(number? expression) *const]
          [(eq? expression #t) *const]
          [(eq? expression #f) *const]
          [(eq? expression 'cons) *const]
          [(eq? expression 'car) *const]
          [(eq? expression 'cdr) *const]
          [(eq? expression 'null?) *const]
          [(eq? expression 'eq?) *const]
          [(eq? expression 'atom?) *const]
          [(eq? expression 'zero?) *const]
          [(eq? expression 'add1) *const]
          [(eq? expression 'sub1) *const]
          [(eq? expression 'number?) *const]
          [else *identifier]
          )))

(define list_analysis
  (lambda (expression)
    (cond [(not(atom? (car expression))) *app]
          [(eq? (car expression) 'quote) *quote]
          [(eq? (car expression) 'lambda) *lambda]
          [(eq? (car expression) 'cond) *cond]
          [else *app]
          )))











