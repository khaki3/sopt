(define-module sopt.data
  (use gauche.record)
  (use util.match)
  (export-all))
(select-module sopt.data)

(define SOPT_GENSYM_COUNT 0)

(define (sopt-gensym :optional (prefix #f))
  (begin0
    (string->symbol
     (string-append
      (if prefix (x->prefix) "")
      (if prefix "--"        "")
      "sopt--"
      (number->string SOPT_GENSYM_COUNT)))

    (inc! SOPT_GENSYM_COUNT)))

;;;
;;; sopt-args
;;;

(define-constant SOPT_UNDEF (gensym))

(define (undef? x)
  (eq? SOPT_UNDEF x))

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
(define-record-type sopt-cxt #t #t ht)

(define (port->sopt-cxt iport)
  (make-sopt-cxt
   (hash-table 'eq?
     (port->list
      (compose (^[x] (if (eof-object? x) x (sopt-parse x))) read)
      iport))))

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
;; cxt     ::= hashtable(name -> def)
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
;;;;;;;;;;;;

(define-record-type sopt-def     #t #f name args terms)

(define-record-type sopt-literal #t #t value)

(define-record-type sopt-if      #t #t test then else)

(define-record-type sopt-let     #t #t bindings terms)

(define-record-type sopt-apply   #t #t proc args)

(define-record-type sopt-call/cc #t #t proc)

(define-record-type sopt-lambda  #t #t frame args terms)

(define-record-type sopt-call    #t #t proc args)

(define-record-type sopt-set!    #t #t var term)

(define (sopt-parse def)
  (match def
    [(or ('define (name . args) . terms)
         ('define name ('lambda args . terms)))
     (make-sopt-def name args (map sopt-parse-term terms))]))

(define-syntax sopt-var?
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

    [(proc-term . args-term)
     (make-sopt-call
      (sopt-parse-term proc-term)
      (map sopt-parse-term args-term))]

    [else
     (if (symbol? term) term (make-sopt-literal term))]))

(define (sopt-deparse def)
  `(define (,(sopt-def-name def) . ,(sopt-def-args def))
     . ,(map sopt-deparse-term (sopt-def-terms def))))

(define (sopt-deparse-term term)
  (cond
   [(sopt-literal? term)
    `(quote ,(sopt-literal-value term))]

   [(sopt-if? term)
    `(if ,(sopt-deparse-term (sopt-if-test term))
         ,(sopt-deparse-term (sopt-if-then term))
         ,(sopt-deparse-term (sopt-if-else term)))]

   [(sopt-let? term)
    `(let
      ,(map
        (lambda (b)
          (list (car b) (sopt-deparse-term (cdr b))))
        (sopt-let-bindings term))
      . ,(map sopt-deparse-term (sopt-let-terms term)))]

   [(sopt-apply? term)
    `(apply ,(sopt-deparse-term (sopt-apply-proc term))
            ,(sopt-deparse-term (sopt-apply-args term)))]

   [(sopt-call/cc? term)
    `(call/cc ,(sopt-deparse-term (sopt-call/cc-proc term)))]

   [(sopt-lambda? term)
    ;; todo: frame expanding
    `(lambda ,(sopt-lambda-args term)
       . ,(map sopt-deparse-term (sopt-lambda-terms term)))]

   [(sopt-set!? term)
    `(set! ,(sopt-set!-var term) ,(sopt-deparse-term (sopt-set!-term term)))]

   [(sopt-call? term)
    `(,(sopt-deparse-term (sopt-call-proc term))
      . ,(map sopt-deparse-term (sopt-call-args term)))]

   [else term]))
