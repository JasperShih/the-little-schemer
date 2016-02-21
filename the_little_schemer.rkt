#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x))
         )))

(define lat?
  (lambda (lst)
    (cond [(null? lst) #t]
          [(atom? (car lst)) (lat? (cdr lst))]
          [else #f]
          )))

(define member?
  (lambda (sym lst)
    (cond [(null? lst) #f]
          [(eq? (car lst) sym) #t]
          [else (member? sym (cdr lst))]
          )))

(define member1?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (lambda (sym lst)
    (cond [(null? lst) '()]
          [(eq? (car lst) sym) (cdr lst)]
          [else (cons (car lst) (rember sym (cdr lst)))]
          )))

(define first
  (lambda (lst)
    (cond [(null? lst) '()]
          [else (cons (caar lst) (first (cdr lst)))]
          )))

(define insertR
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
          [else (cons (car lat) (insertR new old (cdr lat)))]
          )))

(define insertL
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cons new lat)]
          [else (cons (car lat) (insertL new old (cdr lat)))]
          )))

(define subst
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cons new (cdr lat))]
          [else (cons (car lat) (subst new old (cdr lat)))]
          )))

(define subst2
  (lambda (new old1 old2 lat)
    (cond [(null? lat) '()]
          [(or (eq? (car lat) old1)
               (eq? (car lat) old2)) (cons new (cdr lat))]
          [else (cons (car lat) (subst2 new old1 old2 (cdr lat)))]
          )))

(define multi_rember
  (lambda (target lst)
    (cond [(null? lst) '()]
          [(eq? (car lst) target) (multi_rember target (cdr lst))]
          [else (cons (car lst) (multi_rember target (cdr lst)))]
          )))

(define multi_insertR
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cons old
                                     (cons new
                                           (multi_insertR new old (cdr lat))))]
          [else (cons (car lat) (multi_insertR new old (cdr lat)))]
          )))

(define multi_insertL
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cons new (cons old (multi_insertL new old (cdr lat))))]
          [else (cons (car lat) (multi_insertL new old (cdr lat)))]
          )))

(define multi_subst
  (lambda (new old lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) old) (cons new (multi_subst new old (cdr lat)))]
          [else (cons (car lat) (multi_subst new old (cdr lat)))]
          )))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

;(define my_add
 ; (lambda (addend addtion)
  ;  (cond [(zero? addtion) addend]
   ;       [else (my_add (add1 addend) (sub1 addtion))]
    ;      )))

(define my_add
  (lambda (addend addtion)
    (cond [(zero? addtion) addend]
          [else (add1 (my_add addend (sub1 addtion)))]
          )))

(define my_sub
  (lambda (subtrahend minuend)
    (cond [(zero? minuend) subtrahend]
          [else (sub1 (my_sub subtrahend (sub1 minuend)))]
          )))


;(multi_subst 'j 'd '(d a b d c d))

(my_sub 5 7)


















