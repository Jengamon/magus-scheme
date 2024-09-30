(import 
    (only (scheme base)
          begin define define-syntax syntax-rules set! +)
    (only (scheme write) display)
)

(define-syntax x! (syntax-rules () 
    ((x! val)
        (set! x val))
))
(define-syntax define-x (syntax-rules ()
    ((define-x val)
        (begin 
            (define x val)
            (x! (+ val 1))
            x
        ))
))

;; this should error!
(define x x)
(define-x 6)
(display x)
(display "\n")
;; but if *above* that line, you defined `x`, it would work and the
;; value would be overwritten by (the number you passed in + 1)
