; implementation of some things for SRFI 1
; TODO will we need SRFI 0 (conditional code)?
(import (scheme base))
(begin
  (define (filter check xs)
    (if (null? xs)
        '()
        (if (check (car xs))
            (cons (car xs) (filter check (cdr xs)))
            (filter check (cdr xs)))))
)
(export filter)
