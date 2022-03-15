(foreign-declare "
  #include <stdlib.h>
  #include <stdbool.h>
  #include <regex.h>

  /* Maximum error message length */
  #define MAX_ERR_LEN 128

  regex_t *
  make_regex(int *err, char *pattern, bool igncase, bool ext, bool nl)
  {
    int r;
    int cflags;
    regex_t *re;

    cflags = 0;
    if (igncase) cflags |= REG_ICASE;
    if (ext)     cflags |= REG_EXTENDED;
    if (nl)      cflags |= REG_NEWLINE;

    if (!(re = malloc(sizeof(*re)))) {
      *err = REG_ESPACE;
      return NULL;
    }
    if ((r = regcomp(re, pattern, cflags))) {
      *err = r;
      return NULL;
    }

    return re;
  }

  void
  regex_free(regex_t *re)
  {
    regfree(re);
    free(re);
  }

  char *
  regex_error(regex_t *re, int err)
  {
    char *buf;

    if (!(buf = malloc(MAX_ERR_LEN)))
      return NULL;

    /* XXX: Result may be truncated */
    /* TODO: Call with zero buf to get required length first? */
    regerror(err, re, buf, MAX_ERR_LEN);

    return buf;
  }

  int
  regex_match(regex_t* re, char *string, bool notbol, bool noteol)
  {
    int r;
    int eflags;

    eflags = 0;
    if (notbol) eflags |= REG_NOTBOL;
    if (noteol) eflags |= REG_NOTEOL;

    return regexec(re, string, 0, NULL, eflags);
  }
")

;; Constants from regex.h
(define regex-ok 0)
(define regex-nomatch (foreign-value "REG_NOMATCH" int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convenience type alias for exception type from R7RS.
(define-type exception (struct condition))

;; Error object irritant used to identify {{make-regex}} error
;; objects via the {{regex-error?}} error type predicate.

(define-record-type Regex-Irritant
  (make-regex-irritant)
  regex-irritant?)

;; Error type predicate. Returns {{#t}} if the given {{obj}}
;; is an object raised by the {{make-regex}} procedure to
;; indicate a regex library error condition.

(: regex-error? (* -> boolean : exception))
(define (regex-error? obj)
  (if (error-object? obj)
    (let ((irritants (error-object-irritants obj)))
      (and
        (not (null? irritants))
        (regex-irritant? (car irritants))))
    #f))

;; Convenience method for raising a regex error recognized
;; by the {{regex-error?}} error type predicate.

(: error-regex (string -> *))
(define (error-regex msg)
  (error msg (make-regex-irritant)))

;;> Extracts error condition from given {{regex_t*}} pointer value
;;> and associated error code as returned by {{regcomp(3)}}. This
;;> procedure always raises an error and should only be called
;;> if {{regcomp(3)}} returned {{NULL}}.

(: regex-error (pointer integer -> *))
(define (regex-error regex err-code)
  (define %regex-error
    (foreign-lambda c-string* "regex_error" c-pointer int))

  (let ((err-msg (%regex-error regex err-code)))
    (if err-msg
      (error-regex err-msg)
      (error "out of memory"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wrapper around the {{regex_t*}} raw C pointer, created to allow
;; utilizing CHICKEN type annotations for {{regex_t*}} values.

(define-record-type Regex
  (%%make-regex ptr)
  regex?
  (ptr regex-pointer))

;; Convenience type alias
(define-type regex (struct Regex))

;; Type annotation for Regex type constructor.
(: %%make-regex (pointer -> regex))

;;> Returns a pre-compiled regular expression object for the given
;;> {{pattern}}. The optional arguments {{ignorecase}}, {{extended}}, and
;;> {{newline}} specify whether the case should be ignored during
;;> matching, if extended regular expression syntax should be supported,
;;> and if {{<newline>}} in a pattern should not be treated as an ordinary
;;> character. All optional arguments default to {{#f}}.

(: make-regex (string #!optional boolean boolean boolean -> regex))
(define (make-regex pattern #!optional ignorecase extended newline)
  (define %make-regex
    (foreign-lambda c-pointer "make_regex" (nonnull-c-pointer int) nonnull-c-string bool bool bool))

  (let-location ((err integer 0))
    (let ((re (%make-regex (location err) pattern ignorecase extended newline)))
      (if re
        (begin
          (set-finalizer! re regex-free)
          (%%make-regex re))
        (regex-error re err)))))

;;> Check whether the given {{regex}} is matched by the given
;;> {{string}}. If so {{#t}} is returned, otherwise {{#f}} is returned.
;;> The optional {{notbol}} and {{noteol}} procedure arguments control
;;> whether the first/last character of the input should be considered the
;;> start/end of the line.

(: regex-match? (regex string #!optional integer boolean boolean -> boolean))
(define (regex-match? regex string #!optional (submatches 0) notbol noteol)
  (define %regex-match?
    (foreign-lambda int "regex_match" nonnull-c-pointer nonnull-c-string bool bool))

  (unless (zero? submatches)
    (error "support for submatches not implemented yet"))
  (let* ((p (regex-pointer regex))
         (r (%regex-match? p string notbol noteol)))
    (cond
      ((eqv? r regex-ok) #t)
      ((eqv? r regex-nomatch) #f)
      (else (regex-error p r)))))

;; Frees all resources allocate for a {{regex_t*}} pointer value. Invoked
;; automatically via a CHICKEN Garbage Collector finalizer.

(: regex-free (pointer -> undefined))
(define (regex-free ptr)
  (define %regex-free
    (foreign-lambda void "regex_free" nonnull-c-pointer))

  (%regex-free ptr))
