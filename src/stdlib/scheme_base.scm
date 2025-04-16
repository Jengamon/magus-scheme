; Scheme implementations ("Scheme prelude") for (scheme base)
; Here we define things that are simple and wouldn't cause too much of a performance hit.
; We only have the relevant native modules available
(import (scheme base))
; For now, just (undefined) which is used in the implementation of letrec so
; that getting the name before it's defined is...undefined. But any runtime tricks
; are in this module
(import (magus impl))
(export list not memq memv abs square boolean? boolean=?
    zero? positive? negative? length)
; Sketch functions (to be removed once implemented properly)
; `map` is here b/c it has to be able to support multiple lists which is
; easier in Rust (with the goal of the method) than in Scheme (imo)
(export assq assv map)
(begin
  (define (list . in) in)
  (define (not x) (if x #f #t))
  (define (abs n) (if (< n 0) (- n) n))
  (define (square n) (* n n))
  (define (length xs)
    (define (length-iter a l)
      (if (null? a)
          l
          (length-iter (cdr a) (+ l 1))))
    (length-iter xs 0))
  ; this impl should work bc = keeps numbers exact as long as the input is exact
  ; and we use exact to force the input to be exact
  (define (zero? n)
      ; something that is neither exact nor inexact is not a number!
      ; We do this cuz we don't support number? and friends yet
      (if (or (inexact? n) (exact? n)) (= n 0) #f))
  (define (positive? n)
      ; something that is neither exact nor inexact is not a number!
      ; We do this cuz we don't support number? and friends yet
      (if (or (inexact? n) (exact? n)) (> n 0) #f))
  (define (negative? n)
      ; something that is neither exact nor inexact is not a number!
      ; We do this cuz we don't support number? and friends yet
      (if (or (inexact? n) (exact? n)) (< n 0) #f))
  (define (boolean? b) (if (or (eq? b #f) (eq? b #t)) #t #f))
  (define (boolean=? . lst)
    (define (boolean=-iter v lst)
        (if (null? lst)
            #t
            (if (eq? v (car lst))
                (boolean=-iter v (cdr lst))
                #f)))
    (if (and (not (null? lst)) (boolean? (car lst)))
        (boolean=-iter (car lst) (cdr lst))
        ; rn we just use #f, but it *should* error (maybe??)
        (if (null? lst) #t #f)))
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
  (define (assq x alist)
      ; TODO This is a sketch b/c typechecking on "alist" is required
      ; (list of lists where each member list has length == 2)
      ; (b/c 'It is an error if alist (for “association list”) is not a list of pairs.'
      ; in the report.) We *could* check it (if we support raise-continuable), but it would require
      ; a separate iteration that isn't really worth it compared to just implementing it in Rust.
      (define (assq-iter k ascl)
          (if (null? ascl)
              #f
              (if (eq? k (caar ascl))
                  (car ascl)
                  (assq-iter k (cdr ascl)))))
      (assq-iter x alist))
  (define (memv x lst)
      (define (memv-iter a lst)
          (if (null? lst)
              #f
              (if (eqv? a (car lst))
                  lst
                  (memv-iter a (cdr lst)))))
      (memv-iter x lst))
  (define (assv x alist)
      ; TODO This is a sketch b/c typechecking on "alist" is required
      ; (list of lists where each member list has length == 2)
      ; (b/c 'It is an error if alist (for “association list”) is not a list of pairs.'
      ; in the report.) We *could* check it (if we support raise-continuable), but it would require
      ; a separate iteration that isn't really worth it compared to just implementing it in Rust.
      (define (assv-iter k ascl)
          (if (null? ascl)
              #f
              (if (eqv? k (caar ascl))
                  (car ascl)
                  (assv-iter k (cdr ascl)))))
      (assv-iter x alist))
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
