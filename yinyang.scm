#;(let* ((yin
         ((lambda (cc) (display #\@) cc) (call-with-current-continuation (lambda (c) c))))
       (yang
         ((lambda (cc) (display #\*) cc) (call-with-current-continuation (lambda (c) c)))))
    (yin yang))
; currently as is, we don't support the syntax, so
(import (scheme base))
(import (scheme write))
#;((lambda (yin) ((lambda (yang) (yin yang))
    ((lambda (cc) (display #\*) cc) (call-with-current-continuation (lambda (c) c)))))
    ((lambda (cc) (display #\@) cc) (call-with-current-continuation (lambda (c) c))))
((lambda (yin)
    ((lambda (yang) (yin yang))
     ((lambda (cc) (display #\*) cc) (call-with-current-continuation (lambda (c) c)))))
  ((lambda (cc) (display #\xa) cc) (call-with-current-continuation (lambda (c) c))))
