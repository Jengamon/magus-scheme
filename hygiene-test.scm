(import (scheme small))

(define-syntax my-unless
  (syntax-rules ()
    ((_ condition body ...)
     (if (not condition)
         (begin body ...)))))

(let ((not (lambda (x) x)))
  (my-unless #t
    (display "This should not be printed!")
    (newline)))
