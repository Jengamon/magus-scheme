; Scheme implementations ("Scheme prelude") for (scheme base)
; Here we define things that are simple and wouldn't cause too much of a performance hit.
; We only have the relevant native modules available
(import (scheme base))
(export list not map memq memv)
(begin
  (define (list . in) in)
  (define (not x) (if x #f #t))
  (define (map f xs)
    (if (null? (cdr xs))
        (cons (f (car xs)) '())
        (cons (f (car xs)) (map f (cdr xs)))))
  (define (memq x lst)
      (define (memq-iter a lst)
          (if (null? lst)
              #f
              (if (eq? a (car lst))
                  lst
                  (memq-iter a (cdr lst)))))
      (memq-iter x lst))
  (define (memv x lst)
      (define (memv-iter a lst)
          (if (null? lst)
              #f
              (if (eqv? a (car lst))
                  lst
                  (memv-iter a (cdr lst)))))
      (memv-iter x lst))
  ; TODO member, which uses equal (does a length check, and should error if rest is too long,
  ; so waiting on impls equal?, raise)
  (define-syntax when
    (syntax-rules ()
      ((when test result1 result2 ...) (if test (begin result1 result2 ...)))))
  (define-syntax unless
    (syntax-rules ()
      ((unless test result1 result2 ...) (if (not test) (begin result1 result2 ...)))))
)
; as this is Scheme code, it is specific to the compiler used, so we provide a convenience function to
; get it set up (together with its native module) on a specific compiler and world.
