(define-module sopt.data
  (use gauche.record)
  (export-all))
(select-module sopt.data)

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
