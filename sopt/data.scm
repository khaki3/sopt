(define-module sopt.data
  (use gauche.record)
  (use util.match)
  (export-all))
(select-module sopt.data)

(define-constant SOPT_UNDEF 'sopt--inner--undef)

(define (string->sopt-args str)
  (let ([lst (read-from-string str)])
    (unless (list? lst)
      (error #"Invalid sopt-args from command-line: ~str"))
    (map (lambda (x) (if (eq? x '@undef) SOPT_UNDEF x)) lst)))

;; sopt-cxt is a set of `sopt-fun`s
(define (port->sopt-cxt iport)
  (port->list (compose sopt-parse read) iport))

(define (write-sopt-cxt cxt oport)
  (for-each
   (lambda (f)
     (display (sopt-deparse f) oport)
     (newline oport))
   cxt))

(define (sopt-cxt-ref cxt name)
  (find (^[f] (eq? (fun-name f) name)) cxt))

(define (list->sopt-cxt lst)
  lst)

(define (sopt-parse s)
  (define parse-pat
    (match-lambda
      [() '()]

      [(ca . cd) (cons (make-var ca) (make-var cd))]
      ))

  (define parse-binding
    (match-lambda
     [(v t) (cons (sopt-parse v) (sopt-parse t))]))

  (match s
   [('define (name . args) . body)
    (make-fun name (map sopt-parse args) (map sopt-parse body))]

   [('if t1 t2 t3)
    (make-ifs (sopt-parse t1)
              (sopt-parse t2)
              (sopt-parse t3))]

   [('case t0 . clauses)
    (make-cas (sopt-parse t0)
              (map (^[c] (cons (parse-pat (car c)) (sopt-parse (cadr c))))
                   clauses))]

   [('quote lst) lst]

   [('let bindings . body)
    (make-les (map parse-binding bindings)
              (map sopt-parse body))]

   [(fun . args)
    (make-app (car s) (map sopt-parse (cdr s)))]

   [else
    (if (symbol? s)
        (make-var s)
        s)]
   ))

(define (sopt-deparse p)
  (define deparse-pat
    (match-lambda
      [() '()]

      [(ca . cd) (cons (var-name ca) (var-name cd))]
      ))

  (define deparse-binding
    (match-lambda
     [(v . t) (list (sopt-deparse v) (sopt-deparse t))]))

  (cond
   [(fun? p)
    `(define (,(fun-name p) . ,(map sopt-deparse (fun-args p)))
       . ,(map sopt-deparse (fun-body p)))]

   [(ifs? p)
    `(if ,(sopt-deparse (ifs-test p))
         ,(sopt-deparse (ifs-then p)) ,(sopt-deparse (ifs-else p)))]

   [(cas? p)
    `(case ,(sopt-deparse (cas-key p))
       .
       ,(map
         (^[c]
           (list (deparse-pat (car c)) (sopt-deparse (cdr c))))
         (cas-clauses p)))]

   [(les? p)
    `(let ,(map deparse-binding (les-bindings p))
       .  ,(map sopt-deparse (les-body p)))]

   [(pair? p)
    `(quote ,p)]

   [(app? p)
    `(,(app-fun-name p) . ,(map sopt-deparse (app-args p)))]

   [(var? p)
    (var-name p)]

   [(symbol? p)
    `(quote ,p)]

   [else p]
   ))


;;
;; q ::= d1 .. dM
;;
;; d ::= (define (f v1 .. vN) t1 .. tM)
;;
;; t ::= value
;;     | v                                    [variable]
;;     | (f t1 .. tN)                         [function call]
;;     | (if t1 t2 t3)                        [conditional]
;;     | (case t0 (p1 t1) ..)                 [case-expression]
;;     | (let ((v1 t1) .. (vN tN)) t1 .. tM)  [let-expression]
;;
;; p ::= (ca . cd) | ()
;;

(define-record-type fun #t #t
  name args body)

(define-record-type app #t #t
  fun-name args)

(define-record-type cas #t #t
  key clauses)

(define-record-type ifs #t #t
  test then else)

(define-record-type les #t #t ; let
  bindings body)

(define-record-type var #t #t
  name)

(define-method object-equal? ((a var) (b var))
  (eq? (var-name a) (var-name b)))

;;;;;;;;;;;;
;;
;; program ::= def1 ...
;;
;; def     ::= (define var term)
;;
;; var     ::= symbol
;;
;; literal ::= symbol | ...          [Gauche-type]
;;
;; term    ::= var                   [var]
;;           | literal
;;           | (quote literal)       [literal]
;;           | (if test-term
;;                 then-term
;;                 else-term)        [if]
;;           | (lambda formals term) [lambda]
;;           | (set! var term)       [set!]
;;           | (call/cc term)        [call/cc]
;;           | (letrec
;;               ((var1 term1)
;;                ...
;;                (varN-1 termN-1))
;;               termN)              [letrec]
;;           | (apply term ...)      [apply]
;;           | (term1 ...)           [call]
;;

;; (define-record-type sopt-var     #t #t name)

;; (define-record-type sopt-obj     #t #t value)

;; (define-record-type sopt-if      #t #t test     then else)

;; (define-record-type sopt-letrec  #t #t bindings body)

;; (define-record-type sopt-lambda  #t #t formals  term)

;; (define-record-type sopt-set!    #t #t var      term)

;; (define-record-type sopt-call/cc #t #t proc)

;; (define-record-type sopt-apply   #t #t proc     args)

;;
;; * lambda as a object
;;
