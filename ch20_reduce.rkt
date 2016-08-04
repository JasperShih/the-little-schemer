#lang racket

;;--------------  main logic -------------
;(define interpreter
 ; (lambda (exp)
  ;  (letrec ([exps '(car list)]
   ;          [values (list car list)])
    ;  
     ; (letrec ([value (consult exp exps values 'ERROR)])
      ;  value
       ; )
      
      ;)))
      
(define interpreter
  (lambda (exp)
    (letrec {[exps '(car list)]
       		 [values (list car list)]
    
   	[value (consult exp exps values 'ERROR)]}
     value
    )))


;;--------- complete function & data ------------

(define engage
  (lambda (stuff candidates fail)
    (letrec ([reduce_version (lambda (candidates) 
                                (cond {(null? candidates) fail}
                                  	  {(eq? stuff (car candidates)) (car candidates)}
                         	          {else (reduce_version (cdr candidates))}
                              ))]
             
             [ans (reduce_version candidates)]
             )
      ans
      )))

(define consult
  (lambda (index indices values not_found)
    (letrec ([reduce_ver (lambda (indices values)
                           (cond {(null? indices) not_found}
                                 {(eq? index (car indices)) (car values)}
                                 {else (reduce_ver (cdr indices) (cdr values))}
                                 ))]
             
             [value (reduce_ver indices values)]
             )
      value
      )))

;(consult 'fly '(ball fish fly) '(A B C) 'ERROR)

(interpreter 'car)




