(foreign-declare "
  #include <assert.h>
  #include <stdlib.h>
  #include <stdbool.h>
  #include <regex.h>

  #include <sys/types.h>

  regmatch_t *
  make_submatches(size_t n)
  {
    regmatch_t *r;

    if (!(r = malloc(sizeof(*r) * n)))
      return NULL;

    return r;
  }

  void
  submatches_free(regmatch_t *pmatch)
  {
    free(pmatch);
  }

  regmatch_t *
  submatches_get(size_t nmatch, regmatch_t *pmatch, size_t idx)
  {
    if (idx >= nmatch)
      return NULL;

    return &pmatch[idx];
  }

  ssize_t
  submatch_start(regmatch_t *m)
  {
    return (ssize_t)m->rm_so;
  }

  ssize_t
  submatch_end(regmatch_t *m)
  {
    return (ssize_t)m->rm_eo;
  }

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

  size_t
  regex_subexprs(regex_t *re)
  {
    return re->re_nsub;
  }

  char *
  regex_error(regex_t *re, int err)
  {
    int r;
    char *buf;
    int bufsiz;

    /* Find out how big a buffer is needed and alloc it. */
    bufsiz = regerror(err, re, (char *)NULL, (size_t)0);
    assert(bufsiz > 0);
    if (!(buf = malloc(bufsiz)))
      return NULL;

    r = regerror(err, re, buf, bufsiz);
    assert(r <= bufsiz);
    (void)r; /* NDEBUG */

    return buf;
  }

  int
  regex_exec(regex_t* re, char *string, size_t nmatch, regmatch_t *pmatch, bool notbol, bool noteol)
  {
    int r;
    int eflags;

    eflags = 0;
    if (notbol) eflags |= REG_NOTBOL;
    if (noteol) eflags |= REG_NOTEOL;

    return regexec(re, string, nmatch, pmatch, eflags);
  }
")

;; Constants from regex.h
(define regex-ok 0)
(define regex-nomatch (foreign-value "REG_NOMATCH" int))

;; Type alias for R7RS bytevectors (not exported by the R7RS egg).
;; See: https://bugs.call-cc.org/ticket/1796
(define-type bytevector u8vector)

;; Wrapper around the {{regex_t*}} raw C pointer, created to allow
;; utilizing CHICKEN type annotations for {{regex_t*}} values.

(define-record-type Regex
  (%make-regex ptr)
  regex?
  (ptr regex-pointer))

;; Convenience type alias
(define-type regex (struct Regex))

;; Type annotations for Regex type constructor and accessors.
(: %make-regex (pointer -> regex))
(: regex-pointer (regex -> pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wrapper around {{regmatch_t*}} raw C pointer which additionally
;; tracks the amount of allocated submatches (which must not be equal
;; to the amount of matched submatches).

(define-record-type Submatches
  (%make-submatches ptr count)
  submatches?
  (ptr submatches-pointer)
  (count submatches-count))

;; Type annotations for Submatches type constructor and accessors.
(: %make-submatches (pointer integer -> (struct Submatches)))
(: submatches-pointer ((struct Submatches) -> pointer))
(: submatches-count ((struct Submatches) -> integer))

;; Submatch is either a boolean (#f) for a non-matching optional
;; submatch or a pair of bytevector offsets.
(define-type submatch (or false (pair integer integer)))

;; Convenience type alias for vector of submatches.
(define-type submatch-vector (vector-of submatch))

;; Allocate memory to store the correct amount of submatches for
;; a given regular expression.

(: make-submatches (regex -> (struct Submatches)))
(define (make-submatches regex)
  (define %%make-submatches
    (foreign-lambda c-pointer "make_submatches" size_t))

  (let* ((n (+ (regex-subexprs regex) 1)) ;; reserve space for zero subexpression
         (p (%%make-submatches n)))
    (if p
        (begin
          (set-finalizer! p submatches-free)
          (%make-submatches p n))
      (error "out of memory"))))

;; Free memory allocated for a raw {{regmatch_t*}} pointer. Invoked
;; automatically via a CHICKEN garbage collector finalizer.

(: submatches-free (pointer -> undefined))
(define (submatches-free pointer)
  (define %submatches-free
    (foreign-lambda void "submatches_free" nonnull-c-pointer))

  (%submatches-free pointer))

;; Retrieve a single submatch by index. The zero index refers to the
;; substring that corresponds to the entire regular expression. As such,
;; actual submatches start at index 1.

(: submatches-get ((struct Submatches) integer -> pointer))
(define (submatches-get subm idx)
  (define %submatches-get
    (foreign-lambda c-pointer "submatches_get" size_t nonnull-c-pointer size_t))

  (let* ((ptr (submatches-pointer subm))
         (cnt (submatches-count subm))
         (ret (%submatches-get cnt ptr idx)))
    (if ret
      ret
      (error (string-append "out of bounds submatch: " (number->string idx))))))

;; Retrieve the start byte offset of a given submatch.

(: submatch-start (pointer -> integer))
(define (submatch-start match)
  (define %submatch-start
    (foreign-lambda ssize_t "submatch_start" nonnull-c-pointer))

  (%submatch-start match))

;; Retrieve the end byte offset of a given submatch.

(: submatch-end (pointer -> integer))
(define (submatch-end match)
  (define %submatch-end
    (foreign-lambda ssize_t "submatch_end" nonnull-c-pointer))

  (%submatch-end match))

;; Convert single submatch to a pair or a boolean (in the case
;; of a non-matching optional submatch).

(: ->submatch (pointer -> submatch))
(define (pointer->submatch pointer)
  (let ((start (submatch-start pointer))
        (end   (submatch-end pointer)))
    (if (and (eqv? start -1) (eqv? end -1))
      #f
      (cons start end))))

;; Convert encountered submatches to a vector.

(: submatches->vector ((struct Submatches) -> submatch-vector))
(define (submatches->vector subm)
  (define (%submatches->vector idx vec)
    (if (>= idx (submatches-count subm))
      idx
      (let* ((sptr (submatches-get subm idx)))
        (begin
          (vector-set! vec idx (pointer->submatch sptr))
          (%submatches->vector (+ idx 1) vec)))))

  (let* ((vec (make-vector (submatches-count subm)))
         (matched (%submatches->vector 0 vec)))
    ;; Resize vector to actual amount of matched submatches.
    (vector-copy vec 0 matched)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> Returns a pre-compiled regular expression object for the given
;;> {{pattern}}. The optional arguments {{ignorecase}}, {{extended}}, and
;;> {{newline}} specify whether the case should be ignored during
;;> matching, if extended regular expression syntax should be supported,
;;> and if {{<newline>}} in a pattern should not be treated as an ordinary
;;> character. All optional arguments default to {{#f}}.

(: make-regex (string #!optional boolean boolean boolean -> regex))
(define (make-regex pattern #!optional ignorecase extended newline)
  (define %%make-regex
    (foreign-lambda c-pointer "make_regex" (nonnull-c-pointer int) nonnull-c-string bool bool bool))

  (let-location ((err integer 0))
    (let ((re (%%make-regex (location err) pattern ignorecase extended newline)))
      (if re
        (begin
          (set-finalizer! re regex-free)
          (%make-regex re))
        (regex-error re err)))))

;; Returns amount of subexpressions in given regular expressions.

(: regex-subexprs (regex -> integer))
(define (regex-subexprs regex)
  (define %regex-subexprs
    (foreign-lambda size_t "regex_subexprs" nonnull-c-pointer))

  (%regex-subexprs (regex-pointer regex)))

;; Extracts error condition from given {{regex_t*}} pointer value
;; and associated error code as returned by {{regcomp(3)}}. This
;; procedure always raises an error.

(: regex-error (pointer integer -> noreturn))
(define (regex-error regex err-code)
  (define %regex-error
    (foreign-lambda c-string* "regex_error" c-pointer int))

  ;; Due to the c-string* type specifier, CHICKEN will copy memory
  ;; allocated for the error message to a temporary storage and
  ;; free it automatically.
  (let ((err-msg (%regex-error regex err-code)))
    (if err-msg
      (error (string-append "regex error: " err-msg))
      (error "out of memory"))))

;; Low-Level wrapper around {{regexec(3)}} used internally by both
;; {{regex-exec}} and {{regex-match?}} (see documentation below).
;; Returns {{#t}} if the regex matches, {{#f}} if it doesn't, and raises
;; an error if {{regexec(3)}} failed.

(: %regex-exec (regex string integer (or false pointer) boolean boolean -> boolean))
(define (%regex-exec regex string submatches-count submatches-ptr notbol noteol)
  (define %%regex-exec
    (foreign-lambda int "regex_exec" nonnull-c-pointer nonnull-c-string
                                     size_t c-pointer bool bool))

  (let* ((p (regex-pointer regex))
         (r (%%regex-exec p string submatches-count submatches-ptr notbol noteol)))
    (cond
      ((eqv? r regex-ok) #t)
      ((eqv? r regex-nomatch) #f)
      (else (regex-error p r)))))

;;> Execute the given {{regex}} on the given bytevector {{bv}}. Returns
;;> {{#f}} if the match failed or a vector of matching subexpressions.
;;> In the vector, each element is either {{#f}} (for non-participating
;;> optional submatches) or a pair of bytevector offsets. The first
;;> element in the pair specifies the beginning of the submatch in the
;;> bytevector, the second element specifies the end of the submatch.
;;> If the submatch does not participate in a succesfull match, then
;;> both the start and end index are set to -1.
;;>
;;> The optional {{notbol}} and {{noteol}} procedure arguments control
;;> whether the first/last character of the input should be considered
;;> the start/end of the line.

(: regex-exec (regex bytevector #!optional boolean boolean -> (or false submatch-vector)))
(define (regex-exec regex bv #!optional notbol noteol)
  (let* ((subm (make-submatches regex))
         (scnt (submatches-count subm))
         (sptr (submatches-pointer subm)))
    (if (%regex-exec regex (utf8->string bv) scnt sptr notbol noteol)
      (submatches->vector subm)
      #f)))

;;> Check whether the given {{regex}} is matched by the given
;;> {{string}}. If so {{#t}} is returned, otherwise {{#f}} is returned.
;;> This procedure is essentially a variant of {{regex-exec}} which
;;> supports strings instead of bytevectors directly and thus doesn't
;;> support submatches. Refer to {{regex-exec}} for documentation on
;;> the optional {{notbol}} and {{noteol}} procedure parameters.

(: regex-match? (regex string #!optional boolean boolean -> boolean))
(define (regex-match? regex string #!optional notbol noteol)
  (%regex-exec regex string 0 #f notbol noteol))

;; Frees all resources allocate for a {{regex_t*}} pointer value. Invoked
;; automatically via a CHICKEN garbage collector finalizer.

(: regex-free (pointer -> undefined))
(define (regex-free ptr)
  (define %regex-free
    (foreign-lambda void "regex_free" nonnull-c-pointer))

  (%regex-free ptr))
