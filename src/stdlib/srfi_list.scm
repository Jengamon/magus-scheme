; implementation of some things for SRFI 1
(import (scheme base))
(import (scheme cxr))
; constructors
(export cons list xcons cons* make-list)
; predicates
(export (rename list? proper-list?) pair? not-pair?)
; selectors
(export car+cdr list-ref car cdr caar cdar cadr cddr
        caddr cdadr cddar cdaar cadar caadr cdddr caaar
        cadddr cdddar cdaddr cddadr caaddr caddar cddaar cadadr cdaadr cdadar caaadr caadar cadaar cdaaar cddddr caaaar
        first second third fourth fifth sixth seventh eighth ninth tenth)
; miscellaneous
(export length)
; fold, unfold and map
(export map)
; filtering
(export filter)
; searching
(export member memq memv)
; deleting
(export)
; association lists
(export assq assv)
; set operations on lists
(export)
; primitive side-effects
(export)
(begin
  (define (xcons d a) (cons a d))
  (define (cons* a b . rest)
    (if (null? rest)
        (cons a b)
        (cons a (apply cons* b rest))))
  (define (not-pair? x) (not (pair? x)))
  (define (car+cdr pair) (values (car pair) (cdr pair)))
  (define first car)
  (define second cadr)
  (define third caddr)
  (define fourth cadddr)
  (define (fifth lst)
    (car (cdr (cdr (cdr (cdr lst))))))
  (define (sixth lst)
    (car (cdr (cdr (cdr (cdr (cdr lst)))))))
  (define (seventh lst)
    (car (cdr (cdr (cdr (cdr (cdr (cdr lst))))))))
  (define (eighth lst)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr lst)))))))))
  (define (ninth lst)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr lst))))))))))
  (define (tenth lst)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr lst)))))))))))
  (define (filter check xs)
    (define (filter-iter lst check a)
      (if (null? a)
          (reverse lst)
          (if (check (car a))
              (filter-iter (cons (car a) lst) check (cdr a))
              (filter-iter lst check (cdr a)))))
    (filter-iter '() check xs))
)
