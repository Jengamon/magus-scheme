; Implementation of (scheme cxr) in Scheme
; TODO Figure out how importing this should be done
(import (only (scheme base) define car cdr))
; 3
(export caaar
        caadr cadar cdaar
        caddr cddar cdadr
        cdddr)
(begin
    (define (caaar lst)
        (car (car (car lst))))
    (define (caadr lst)
        (car (car (cdr lst))))
    (define (cadar lst)
        (car (cdr (car lst))))
    (define (cdaar lst)
        (cdr (car (car lst))))
    (define (caddr lst)
        (car (cdr (cdr lst))))
    (define (cddar lst)
        (cdr (cdr (car lst))))
    (define (cdddr lst)
        (cdr (cdr (cdr lst))))
    (define (cdadr lst)
        (cdr (car (cdr lst)))))
; 4
(export caaaar
        caaadr caadar cadaar cdaaar
        caaddr caddar cddaar
        cadddr cdddar
        cddddr)
(begin
    (define (caaaar lst)
        (car (car (car (car lst)))))
    (define (caaadr lst)
        (car (car (car (cdr lst)))))
    (define (caadar lst)
        (car (car (cdr (car lst)))))
    (define (cadaar lst)
        (car (cdr (car (car lst)))))
    (define (cdaaar lst)
        (cdr (car (car (car lst)))))
    (define (caaddr lst)
        (car (car (cdr (cdr lst)))))
    (define (caddar lst)
        (car (cdr (cdr (car lst)))))
    (define (cddaar lst)
        (cdr (cdr (car (car lst)))))
    (define (cadddr lst)
        (car (cdr (cdr (cdr lst)))))
    (define (cdddar lst)
        (cdr (cdr (cdr (car lst)))))
    (define (cddddr lst)
        (cdr (cdr (cdr (cdr lst))))))
