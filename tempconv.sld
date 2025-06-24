(define-library (tempconv)
  (import (only (scheme base) + * define))
  (export c->f f->c)
  (include "tempconv.scm"))
