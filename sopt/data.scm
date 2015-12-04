(define-module sopt.data
  (use gauche.record)
  (use util.match)
  (export-all))
(select-module sopt.data)

;;;
;;; sopt-args
;;;

(define-constant SOPT_UNDEF 'sopt--inner--undef)

;; list as sopt-args
(define-syntax make-sopt-args
  (syntax-rules ()
    [(_ arg ...) (list arg ...)]))

(define (string->sopt-args str)
  (let ([lst (read-from-string str)])
    (unless (list? lst)
      (error #"Invalid sopt-args from command-line: ~str"))
    (map (lambda (x) (if (eq? x '@undef) SOPT_UNDEF x)) lst)))

;;;
;;; sopt-cxt
;;;

;; sopt-cxt is a wrapper of <hash-table>, containing `sopt-def`s
(define-record-type sopt-cxt %make-sopt-cxt #t ht)

(define (port->sopt-cxt iport)
  (%make-sopt-cxt (hash-table 'eq? (port->list (compose sopt-parse read) iport))))

(define (write-sopt-cxt cxt oport)
  (hash-table-for-each (sopt-cxt-ht cxt)
    (lambda (name def)
      (display (sopt-deparse def) oport)
      (newline oport))))

(define (sopt-cxt-ref cxt name)
  (ref (sopt-cxt-ht cxt) name #f))

;;;
;;; syntax
;;;

;;;;;;;;;;;;
;;
;; cxt     ::= hashtable((name . def) ...)
;;
;; def     ::= (define (name . args) term)
;;
;; var     ::= symbol
;;
;; literal ::= symbol | ...          // Gauche-types
;;
;; term    ::= var                   [var]
;;           | literal
;;           | (quote literal)       [literal]
;;           | (if test-term
;;                 then-term
;;                 else-term)        [if]
;;           | (let
;;               ((var1 term1)
;;                ...
;;                (varN-1 termN-1))
;;               . terms)            [let]
;;           | (apply term lst)      [apply]
;;           | (lambda args . terms) [lambda]
;;           | (call/cc term)        [call/cc]
;;           | (set! var term)       [set!]
;;           | (term1 ...)           [call]
;;

(define (sopt-parse def)
  (match def
    [(or ('define (name . args) . terms)
         ('define name ('lambda args . terms)))
     (make-sopt-def name args (map sopt-parse-term terms))]))

(define-syntax (sopt-var? t)
  (syntax-rules ()
    [(_ t) (symbol? t)]))

(define (sopt-parse-term term)
  (match term
    [('quote literal)
     (make-sopt-literal literal)]

    [('if test-term then-term else-term)
     (make-sopt-if
      (sopt-parse-term test-term)
      (sopt-parse-term then-term)
      (sopt-parse-term else-term))]

    [('let bindings . terms)
     ;; use alist as bindings
     (make-sopt-let
      (map
       (lambda (b)
         (let ((b-var (car b)) (b-term (cadr b)))
           (cons b-var (sopt-parse-term b-term))))
       bindings)
      (map sopt-parse-term terms))]

    [('apply t1 t2)
     (make-sopt-apply (sopt-parse-term t1) (sopt-parse-term t2))]

    [(or ('call/cc t) ('call-with-current-continuation t))
     (make-sopt-call/cc (sopt-parse-term t))]

    [('lambda args . terms)
     (make-sopt-lambda #f args (map sopt-parse-term terms))]

    [('set! var t)
     (make-sopt-set! var (sopt-parse-term t))]

    [(term-fun . args-term
     (make-sopt-call
      (sopt-parse-term term-fun)
      (map sopt-parse-term args-term)))]

    [else
     (if (symbol? term) term (make-sopt-literal))]))

(define-record-type sopt-def     #t #f name args terms)

(define-record-type sopt-literal #t #t value)

(define-record-type sopt-if      #t #t test then else)

(define-record-type sopt-let     #t #t bindings terms)

(define-record-type sopt-apply   #t #t proc lst)

(define-record-type sopt-call/cc #t #t proc)

(define-record-type sopt-lambda  #t #t frame args terms)

(define-record-type sopt-set!    #t #t var term)
