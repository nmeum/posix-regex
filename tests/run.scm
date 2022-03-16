(import (r7rs) (test) (posix-regex))

(define (test-match pattern string)
  (regex-match?
    (make-regex pattern)
    string))

(define (test-exec pattern submatches string)
  (let-values (((matched? submatches) (regex-exec
                                        (make-regex pattern)
                                        (string->utf8 string)
                                        submatches)))
    (cons matched? submatches)))

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
  (test "match literal string" #t (test-match "foo" "foo"))
  (test "don't match literal string" #f (test-match "foo" "bar"))
  (test "partially match literal string" #t (test-match "foo" "foobar"))
  (test "match bracket expression" #t (test-match "f[a-z][a-z]b" "foobar"))
  (test "match repeated expression" #t (test-match "a*b" "aaaaab"))
  (test "match start and end" #f (test-match "^foo$" "|foo|")))

(test-group "regex-exec"
  (test "match literal string"
        '(#t . #())
        (test-exec "foo" 0 "foo"))

  (test "not matching"
        '(#f . #())
        (test-exec "foo" 0 "bar"))

  (test "match single submatch"
        '(#t . #((0 . 13) (5 . 8)))
        (test-exec "foo |\\(..*\\)| baz" 1 "foo |bar| baz"))

  (test "match zero-length string"
        '(#t . #((0 . 10) (5 . 5)))
        (test-exec "foo '\\(.*\\)' baz" 1 "foo '' baz"))

  (test "non-participating submatch"
        '(#t . #((0 . 8) (-1 . -1) (5 . 8)))
        (test-exec "foo \\(..*\\)* \\(..*\\)" 2 "foo  baz")))

;; Exit with non-zero exit status if some test failed.
(test-exit)
