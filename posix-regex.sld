(define-library (posix-regex)
  (import (scheme base)

          (chicken type)
          (chicken foreign)
          (chicken gc))

  (export make-regex regex? regex-error?)
  (include "posix-regex.scm"))
