#lang racket

(define interpreter
  (lambda (exp)
    (letrec ([handler (identify_exp exp)]
             [value (generate_meaning_of_exp handler exp)]
             )
      value
      )))

(define identify_exp
  (lambda (exp)
    (letrec ([exp_type (classify_exp_type exp)]
             [handler (get_type_handler exp_type)]
             )      
      handler
      )))
	
(define classify_exp_type
  (lambda (exp)
    (letrec ([exp_kind (list_or_atom? exp)]
             [type (match_with_exp_kind exp_kind exp)]
             
             [list_or_atom? (lambda (exp)
                              (cond ((F_atom? exp) 'atom)
                                    (else 'list)
                                    ))]
             [match_with_exp_kind (lambda (exp_kind exp)
                                  (cond ((eq? exp_kind 'atom) (match_with_atom_exps exp))
                                        (else (match_with_list_exps exp))
                                        ))]
             )
      type
      )))


(define match_with_atom_exps
  (lambda (exp)
    (letrec ([type matching]
             
             [matching (cond {(number? exp) 'number}
                         	 {else (match_map exp atom_list type_list)}
                           )]
             [match_map (lambda (exp lst val_list)
                                     (cond {(null? lst) 'identifier}
                                           {(eq? exp (car lst)) (car val_list)}
                                           {else (match_map exp (cdr lst) (cdr val_list))}
                                           ))]
             [atom_list '(#t #f cons car cdr null? eq? atom? zero? number?)]
             [type_list '(boolean boolean cons car cdr null? eq? atom? zero? number?)]
             )
      type
      )))

(define match_with_list_exps
  (lambda (exp)
    (letrec ([]
             )
      ...
      )))
 



(define get_type_handler
  (lambda (type)
    (letrec ([handler ...
              		  ...]
             )
      handler
      )))

(define generate_meaning_of_exp
  (lambda (handler exp)
    (handler exp)))


value interpreter (exp):
	handler identify_exp (exp)
 		exp_type classify_exp_type (exp)
 		handler get_type_handler (exp_type)
 	value generate_meaning_of_exp (handler exp)
  
  
(define F_atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x))
     )))

  

 
  































	