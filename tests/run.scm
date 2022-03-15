(import (r7rs) (test) (posix-regex))

(define (test-regex regex string)
  (regex-match?
    (make-regex regex)
    string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "make-regex"
  (test-assert "literal string" (regex? (make-regex "foo")))
  (test-assert "ignorecase" (regex? (make-regex "foobar" #t)))
  (test-assert "extended regular expression" (regex? (make-regex "[0-9]+" #f #t)))
  (test-assert "newline option" (regex? (make-regex "foo" #f #f #t)))

  (test-error "invalid interval expression" (make-regex "\\{foo,foo\\}")) ;; REG_BADAR
  (test-error "parentheses imbalance" (make-regex "\\(foo"))              ;; REG_EBRACE
  (test-error "bracket imbalance" (make-regex "["))                       ;; REG_EBRACK
  (test-error "trailing backslash" (make-regex "\\")))                    ;; REG_EESCAPE

(test-group "regex-match?"
  (test "match literal string" #t (test-regex "foo" "foo"))
  (test "don't match literal string" #f (test-regex "foo" "bar"))
  (test "partially match literal string" #t (test-regex "foo" "foobar"))
  (test "match bracket expression" #t (test-regex "f[a-z][a-z]b" "foobar"))
  (test "match repeated expression" #t (test-regex "a*b" "aaaaab"))
  (test "match start and end" #f (test-regex "^foo$" "|foo|")))

;; Exit with non-zero exit status if some test failed.
(test-exit)
