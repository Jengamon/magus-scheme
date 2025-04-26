; implementation of some things for SRFI 1
(import (scheme base))
(import (scheme cxr))
; constructors
(export cons list xcons cons* make-list)
; predicates
(export (rename list? proper-list?) pair? not-pair?)
; selectors
(export car+cdr car cdr caar cdar cadr cddr)
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
  (define (filter check xs)
    (if (null? xs)
        '()
        (if (check (car xs))
            (cons (car xs) (filter check (cdr xs)))
            (filter check (cdr xs)))))
)
