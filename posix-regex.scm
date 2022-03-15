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
")

;; Convenience type alias for exception type from R7RS.
(define-type exception (struct condition))

(define-record-type Regex-Irritant
  (make-regex-irritant)
  regex-irritant?)

(: regex-error? (* -> boolean : exception))
(define (regex-error? obj)
  (if (error-object? obj)
    (let ((irritants (error-object-irritants obj)))
      (and
        (not (null? irritants))
        (regex-irritant? (car irritants))))
    #f))

(: error-regex (string -> *))
(define (error-regex msg)
  (error msg (make-regex-irritant)))

(: regex-error (pointer integer -> *))
(define (regex-error regex err-code)
  (define %regex-error
    (foreign-lambda c-string* "regex_error" c-pointer int))

  (let ((err-msg (%regex-error regex err-code)))
    (if err-msg
      (error-regex err-msg)
      (error "out of memory"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Regex
  (%%make-regex ptr)
  regex?
  (ptr regex-pointer))

;; Convenience type alias
(define-type regex (struct Regex))

;; Type annotation for Regex type constructor.
(: %%make-regex (pointer -> regex))

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

(: regex-free (pointer -> undefined))
(define (regex-free ptr)
  (define %regex-free
    (foreign-lambda void "regex_free" nonnull-c-pointer))

  (%regex-free ptr))
