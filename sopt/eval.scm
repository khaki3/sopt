(define-module sopt.eval
  (use sopt.data)
  (use sopt.ext)
  (use util.match)
  (use gauche.record)
  (export sopt-eval))
(select-module sopt.eval)

(define-record-type sopt-info %make-sopt-info #f
  cxt
  ext
  bind ; hashtable((name, args) -> new-name)
  opt  ; hashtable(name -> optimized-def)
  )

(define (make-sopt-info cxt ext)
  (%make-sopt-info
   cxt
   ext
   (make-hash-table 'equal?)
   (make-hash-table 'eq?)))

(define (info->cxt info)
  (make-sopt-cxt (sopt-info-opt info)))

(define (info-bind! info name args new-name)
  (hash-table-put! (sopt-info-bind info) (cons name args) new-name))

(define (info-bind-ref info name args)
  (ref (sopt-info-bind info) (cons name args) #f))

(define (info-add-opt! info name def)
  (hash-table-put! (sopt-info-opt info) name def))

(define (info-opt-ref info name)
  (ref (sopt-info-opt info) name #f))

(define (info-cxt-ref info name)
  (sopt-cxt-ref (sopt-info-cxt info) name))

#|
   target-args   [from command-line]
     SOPT_UNDEF, var, literal

   template-args [arguments of definition]
     var

   actual-args   [run-time args]
     var, literal

   plain-args    [for hash-table's key]
     SOPT_UNDEF, literal-value
|#

;; target-args -> actual-args
;; replace undef by template-args
(define (enable-args info target target-args)
  (and-let1 target-def (sopt-cxt-ref (sopt-info-cxt info) target)
    (map
     (lambda (templ targ)
       (if (undef? targ) templ targ))
     (sopt-def-args target-def) target-args)))

;; actual-args -> plain-args
(define (normalize-args actual-args)
  (map
   (lambda (a)
     (if (sopt-var? a) SOPT_UNDEF
         (sopt-literal-value a)))
   actual-args))

(define (sopt-eval cxt ext target target-args)
  (let1 info (make-sopt-info cxt ext)
    (sopt-opt! info target (enable-args info target target-args))
    (info->cxt info)))

;; delete the elements without var
(define (reduce-args actual-args)
  (filter-map
   (lambda (a) (and (sopt-var? a) a))
   actual-args))

(define-method make-sopt-env (template-args actual-args)
  (map
   (lambda (t a)
     (cons t (if (sopt-var? a)
                 (make-sopt-trace a SOPT_UNDEF)
                 (make-sopt-trace t a))))
   template-args actual-args))

(define (sopt-opt! info name actual-args)
  (and-let1 def (info-cxt-ref info name)
     (let* ([plain-args    (normalize-args actual-args)]
            [origin        (every undef? plain-args)]
            [new-name      (if origin name (sopt-gensym name))]
            [template-args (sopt-def-args def)]
            [env           (make-sopt-env template-args actual-args)])
       (info-bind! info name plain-args new-name)

       (rlet1 new-def
          (make-sopt-def
           name
           (reduce-args actual-args)
           (map (cut drive info <> env) (sopt-def-terms def)))

          (info-add-opt! info new-name new-def)
          ))))

(define (drive-map info terms env)
  (map (cut drive info <> env) terms))

(define (drive info term env)
  (let-syntax
      ((drive-distribute
        (syntax-rules ()
          ((_ (predicate proc) ...)
           (cond ((predicate term) (proc info term env)) ...)))))

    (drive-distribute
     ;(sopt-set!     drive-set!)
     ;(sopt-if?      drive-if)
     ;(sopt-lambda?  drive-lambda)
     ;(sopt-call/cc? drive-call/cc)
     (sopt-call?    drive-call)
     (sopt-apply?   drive-apply)
     (sopt-let?     drive-let)
     (sopt-var?     drive-var)
     (sopt-literal? drive-literal))))

(define (drive-call info term env)
  term)

(define (drive-apply info term env)
  term)

(define (drive-let info term env)
  (let* ([bindings (sopt-let-bindings term)]
         [declared (map car bindings)])

    ;;
    ;; `declared` is prepared for the code like below
    ;;
    ;; (let (( a (F) ))
    ;;   (let (( a (G) )
    ;;         ( b  a  )) ; this binding must remain.
    ;;
    ;;     b  ; must be the result of (F)
    ;;
    ;;     ))
    ;;

    (let loop ([bindings     bindings]
               [new-bindings '()]
               [new-env      env])

      (if (null? bindings)
          (make-sopt-let
           new-bindings
           (drive-map info (sopt-let-terms term) new-env))

          (let* ([b-var  (caar bindings)]
                 [b-term (drive info (cdar bindings) env)]
                 [disappearable-var
                  (and (sopt-var? b-term) (not (memq b-term declared)))]

                 [new-env
                  (add-trace new-env b-var
                    (cond [disappearable-var
                           (or (sopt-env-ref env b-term)
                               (make-sopt-trace b-term SOPT_UNDEF))]

                          [(sopt-literal? b-term)
                           (make-sopt-trace b-var b-term)]

                          [else
                           (make-sopt-trace b-var SOPT_UNDEF)]))]

                 [new-bindings
                  (if (or disappearable-var (sopt-literal? b-term))
                      new-bindings (cons (cons b-var b-term) new-bindings))])

            (loop (cdr bindings) new-bindings new-env))))))

(define (drive-var info term env)
  (if-let1 trace (sopt-env-ref env term)
     (let ([var ((car trace))]
           [val ((cdr trace))])
       (if (undef? val) var val))
     term))

(define (drive-literal info term env) term)
