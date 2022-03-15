(define-library (posix-regex)
  (import (scheme base)

          (chicken type)
          (chicken foreign)
          (chicken gc))

  (export make-regex regex? regex-exec regex-match?)
  (include "posix-regex.scm"))
