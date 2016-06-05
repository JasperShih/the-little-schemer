#lang racket

(define bons
  (lambda (element)
    (let [(complement '())]
      (lambda (function)
        (function
         (lambda (x) (set! complement x))
         element
         complement))
      )
    ))

(define kar
  (lambda (c)
    (c (lambda (a b c) b))))

(define kdr
  (lambda (c)
    (c (lambda (a b c) c))))

(define set_kdr
  (lambda (lst new_complement)
    ((lst (lambda (a b c) a))
     new_complement)))

(define kons
  (lambda (cell others)
    [let ((structure (bons cell))
          )
      (set_kdr structure others)
      structure
      ]))



;(define my_lst (kons 'egg '()))
;(kar my_lst)
;(kdr my_lst)

;(set_kdr my_lst '(gg))
;(kar my_lst)
;(kdr my_lst)

(define lots
  (lambda (m)
    (cond [(zero? m) '()]
          [else (kons 'egg (lots (- m 1)))]
          )))

(define len
  (lambda (lst)
    (cond [(null? lst) 0]
          [else (+ 1 (len (kdr lst)))]
          )))

(define last_kons
  (lambda (structure)
    (cond [(null? (kdr structure)) structure]
          [else (last_kons (kdr structure))]
          )))

;(define long (lots 2))

;(set_kdr (last_kons long) (kdr (kdr long)))

;(len long)

(define same?
  (lambda (c1 c2)
    [let ((t1 (kdr c1))
          (t2 (kdr c2))
          )
      (set_kdr c1 1)
      (set_kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set_kdr c1 t1)
        (set_kdr c2 t2)
        v
        )
      ]))

;(same? (kons 'egg '()) (kons 'egg '()))

(define finite_len
  (lambda (p)
    (let/cc infinite
      [letrec ((C (lambda (p q)
                    (cond [(same? p q) (infinite #f)]
                          [(null? q) 0]
                          [(null? (kdr q)) 1]
                          [else (+ (C (sl p) (qk q))
                                   2)]
                          )))
               (qk (lambda (x) (kdr (kdr x))))
               (sl (lambda (x) (kdr x)))
               )
        (cond [(null? p) 0]
              [else (+ 1 (C p (kdr p)))])
        ])))

(define long (lots 12))
(finite_len long)
(set_kdr (last_kons long) long)
(finite_len long)

(define mongo
  (kons 'pie
        (kons 'a
              (kons 'la
                    (kons 'mode '())))))



























