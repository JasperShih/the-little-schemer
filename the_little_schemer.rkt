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

(define addtup
  (lambda (tup)
    (cond [(null? tup) 0]
          [else (my_add (car tup) (addtup (cdr tup)))]
          )))

(define my_multiply
  (lambda (multiplicand multiplier)
    (cond [(zero? multiplier) 0]
          [else (my_add multiplicand (my_multiply multiplicand (sub1 multiplier)))]
          )))

(define tup+
  (lambda (tup1 tup2)
    (cond [(null? tup1) tup2]
          [(null? tup2) tup1]
          [else (cons (my_add(car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))]
          )))

(define larger?
  (lambda (n m)
    (cond [(zero? n) #f]
          [(zero? m) #t]
          [else (larger? (sub1 n) (sub1 m))]
          )))

(define smaller?
  (lambda (n m)
    (cond [(zero? m) #f]
          [(zero? n) #t]
          [else (smaller? (sub1 n) (sub1 m))]
          )))

(define =
  (lambda (n m)
    (cond [(larger? n m) #f]
          [(smaller? n m) #f]
          [else #t]
          )))

(define power
  (lambda (base times)
    (cond [(zero? times) 1]
          [(= times 1) base]
          [(zero? base) 0]
          [(= base 1) 1]
          [else (my_multiply base (power base (sub1 times)))]
          )))

(define length
  (lambda (lst)
    (cond [(null? lst) 0]
          [else (add1 (length (cdr lst)))]
          )))

(define pick
  (lambda (index lst)
    (cond [(= index 1) (car lst)]
          [else (pick (sub1 index) (cdr lst))]
          )))

(define rempick
  (lambda (index lst)
    (cond [(= index 1) (cdr lst)]
          [else (cons (car lst) (rempick (sub1 index) (cdr lst)))]
          )))

(define no_nums
  (lambda (lst)
    (cond [(null? lst) '()]
          [(number? (car lst)) (no_nums (cdr lst))]
          [else (cons (car lst) (no_nums (cdr lst)))]
          )))


(define all_nums
  (lambda (lst)
    (cond [(null? lst) '()]
          [(number? (car lst)) (cons (car lst) (all_nums (cdr lst)))]
          [else (all_nums (cdr lst))]
          )))

(define eqan?
  (lambda (atom1 atom2)
    (cond [(and (number? atom1) (number? atom2)) (= atom1 atom2)]
          [(or (number? atom1) (number? atom2)) #f]
          [else (eq? atom1 atom2)]
          )))

(define one?
  (lambda (n)
    (cond [else (= n 1)]
          )))

(define rember*
  (lambda (target lst)
    (cond [(null? lst) lst]
          [(not(atom? (car lst))) (cons (rember* target (car lst))
                                   (rember* target (cdr lst)))]
          [(eqv? target (car lst)) (rember* target (cdr lst))]
          [else (cons (car lst) (rember* target (cdr lst)))]
          )))

(define insertR*
  (lambda (new old lst)
    (cond [(null? lst) '()]
          [(not(atom? (car lst))) (cons (insertR* new old (car lst))
                                        (insertR* new old (cdr lst)))]
          [(eqv? old (car lst)) (cons old (cons new (insertR* new old (cdr lst))))]
          [else (cons (car lst) (insertR* new old (cdr lst)))]
          )))

(define occur*
  (lambda (target lst)
    (cond [(null? lst) 0]
          [(not(atom? (car lst))) (my_add (occur* target (car lst))
                                          (occur* target (cdr lst)))]
          [(eqv? (car lst) target) (my_add 1 (occur* target (cdr lst)))]
          [else (occur* target (cdr lst))]
          )))

(define subst*
  (lambda (new old lst)
    (cond [(null? lst) '()]
          [(not (atom? (car lst))) (cons(subst* new old (car lst))
                                  (subst* new old (cdr lst)))]
          [(eqv? (car lst) old) (cons new (subst* new old (cdr lst)))]
          [else (cons (car lst) (subst* new old (cdr lst)))]
          )))

(define insertL*
  (lambda (new old lst)
    (cond [(null? lst) '()]
          [(not (atom? (car lst))) (cons (insertL* new old (car lst))
                                         (insertL* new old (cdr lst)))]
          [(eqv? (car lst) old) (cons new (cons old (insertL* new old (cdr lst))))]
          [else (cons (car lst) (insertL* new old (cdr lst)))]
          )))


(define member*
  (lambda (target lst)
    (cond [(null? lst) #f]
          [(not(atom? (car lst))) (or (member* target (car lst))
                                     (member* target (cdr lst)))]
          [(eqv? (car lst) target) #t]
          [else (member* target (cdr lst))]
          )))

(define leftmost
  (lambda (lst)
    (cond [(not (atom? (car lst))) (leftmost (car lst))]
          [else (car lst)]
          )))

;;Scheme裡的or只要執行到最前項是true的, 後面的項就不執行了, 直接return true.
;;Scheme裡的and只要執行到最前項是false的, 後面的項就不執行了, 直接return false.


(define eqlist?
  (lambda (lst1 lst2)
    (cond [(and (null? lst1) (null? lst2)) #t]
          [(or (null? lst1) (null? lst2)) #f]
          [(and (not(atom?(car lst1)))
                (not(atom?(car lst2)))) (and (eqlist? (car lst1) (car lst2))
                                            (eqlist? (cdr lst1) (cdr lst2)))]
          [(or (not(atom?(car lst1)))
                (not(atom?(car lst2)))) #f]
          [(eqan? (car lst1) (car lst2)) (eqlist? (cdr lst1) (cdr lst2))]
          [else #f]
          )))


(define equal?
  (lambda (sexp1 sexp2)
    (cond [(and (atom? sexp1) (atom? sexp2)) (eqan? sexp1 sexp2)]
          [(or (atom? sexp1) (atom? sexp2)) #f]
          ;;There are two list of S-expression
          [else (eqlist? sexp1 sexp2)]
          )))

(define eqlist2?
  (lambda (lst1 lst2)
    (cond [(or (atom? lst1) (atom? lst2)) #f]
          [else (equal? lst1 lst2)]
          )))

;;is a number expression?
(define numbered?
  (lambda (exp)
    (cond [(atom? exp) (number? exp)]
          [(or (eq? (cadr exp) '+)
               (eq? (cadr exp) '*)
               (eq? (cadr exp) '^)) {and (number? (car exp))
                                         (number? (caddr exp))}]
          [else #f]
          )))

(define value
  (lambda (exp)
    (cond [(number? exp) exp]
          [(pair? (car exp)) (value (cons (value (car exp)) (cdr exp)))]
          [(eqv? (length exp) 1) (car exp)]
          [(eq? (cadr exp) '+) (value (cons (+ (car exp) (caddr exp)) (cdddr exp)))]
          [(eq? (cadr exp) '*) (value (cons (* (car exp) (caddr exp)) (cdddr exp)))]
          [(eq? (cadr exp) '^) (value (cons (power (car exp) (caddr exp)) (cdddr exp)))]
          )))


(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define edd
  (lambda (summand addend)
    (cond [(sero? addend) summand]
          [else (edd1 (edd summand (zub1 addend)))]
          )))

(define union
  (lambda (set1 set2)
    (cond [(null? set1) set2]
          [(member? (car set1) set2) (cons (car set1)
                                           (union (cdr set1) (rember (car set1) set2)))]
          [else (cons (car set1)
                      (union (cdr set1) set2))]
          )))


(define intersect
  (lambda (set1 set2)
    (cond [(null? set1) '()]
          [(member? (car set1) set2) (cons (car set1)
                                           (intersect (cdr set1) set2))]
          [else (intersect (cdr set1) set2)]
          )))



(define intersectall
  (lambda (lst)
    (cond[(null? (cdr lst)) (car lst)]
         [else (intersect(car lst)
                         (intersectall (cdr lst)))]
         )))

(define a_pair?
  (lambda (x)
    (cond[(null? x) #f]
         [(null? (cdr x)) #f]
         [(null? (cddr x)) #t]
         [else #f]
         )))

(define revrel
  (lambda (rel)
    (cond[(null? rel) '()]
         [else (cons (cons (cadr (car rel))
                           (cons (car (car rel))
                                 '()))
                     (revrel (cdr rel)))]
         )))


(define rember_f
  (lambda (test? a lst)
    (cond[(null? lst) '()]
         [(test? (car lst) a) (cdr lst)]
         [else (cons (car lst)
                     (rember_f test? a (cdr lst)))]
         )))

(define eq?_c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember_f2
  (lambda (test?)
    (lambda (a lst)
      (cond[(null? lst) '()]
           [(test? (car lst) a) (cdr lst)]
           [else (cons (car lst)
                     ((rember_f2 test?) a (cdr lst)))]
           ))))


(define insertL_f
  (lambda (test?)
    (lambda (new old lat)
      (cond[(null? lat) '()]
           [(test? (car lat) old) (cons new lat)]
           [else (cons (car lat) ((insertL_f test?) new old (cdr lat)))]
           ))))


(define insertR_f
  (lambda (test?)
    (lambda (new old lat)
      (cond[(null? lat) '()]
           [(test? (car lat) old) (cons old (cons new (cdr lat)))]
           [else (cons (car lat) ((insertR_f test?) new old (cdr lat)))]
           ))))


(define insert_g
  (lambda (seq)
    (lambda (new old lat) 
      (cond[(null? lat) '()]
           [(eq? (car lat) old) (seq new old lat)]
           [else (cons (car lat) ((insert_g seq) new old (cdr lat)))]
           ))))

(define left
  (lambda (new old lat)
    (cons new lat)))

(define right
  (lambda (new old lat)
    (cons old (cons new (cdr lat)))))

;(define insert_L (insert_g left))
;(define insert_R (insert_g right))

(define insert_L
  (insert_g
   (lambda (new old lat) (cons new lat)))) ;;此處定義了什麼是seq

(define insert_R
  (insert_g
   (lambda (new old lat) (cons old (cons new (cdr lat)))))) ;;此處定義了什麼是seq

(define insert_g_subst
  (insert_g (lambda (new old lat)
              (cons new (cdr lat)))))

(insert_g_subst ' 7788 'jelly '(ggc jelly beans are good))









































