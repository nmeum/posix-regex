(import (r7rs) (test) (posix-regex))

(test-group "make-regex"
  (test-assert "literal string" (regex? (make-regex "foo")))
  (test-assert "ignorecase" (regex? (make-regex "foobar" #t)))
  (test-assert "extended regular expression" (regex? (make-regex "[0-9]+" #f #t)))
  (test-assert "newline option" (regex? (make-regex "foo" #f #f #t)))

  (test-error "invalid interval expression" (make-regex "\\{foo,foo\\}")) ;; REG_BADAR
  (test-error "parentheses imbalance" (make-regex "\\(foo"))              ;; REG_EBRACE
  (test-error "bracket imbalance" (make-regex "["))                       ;; REG_EBRACK
  (test-error "trailing backslash" (make-regex "\\")))                    ;; REG_EESCAPE

;; Exit with non-zero exit status if some test failed.
(test-exit)
