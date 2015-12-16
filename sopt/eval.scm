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

(define (info-already-binded? info name args)
  (hash-table-exists? (sopt-info-bind info) (cons name args)))

(define (info-add-opt! info name def)
  (hash-table-put! (sopt-info-opt info) name def))

(define (sopt-eval cxt ext target target-args)
  (let1 info (make-sopt-info cxt ext)
    (sopt-opt! info target target-args)
    (info->cxt info)))

(define (reduce-args template-args actual-args)
  (filter-map
    (lambda (t a) (and (undef? a) t))
    template-args actual-args))

(define-method make-sopt-env (template-args actual-args)
  (map
   (lambda (t a) (cons t (make-sopt-trace t a)))
   template-args actual-args))

(define (sopt-opt! info target actual-args)
  (or (info-already-binded? info target actual-args)

      (and-let1 target-def (sopt-cxt-ref (sopt-info-cxt info) target)
        (let* ([origin        (every undef? actual-args)]
               [name          (if origin target (sopt-gensym target))]
               [template-args (sopt-def-args target-def)]
               [env           (make-sopt-env template-args actual-args)])
          (info-bind! info target actual-args name)

          (info-add-opt! info name
            (make-sopt-def
             name
             (reduce-args template-args actual-args)
             (map (cut drive info <> env) (sopt-def-terms target-def))
             ))))))

(define (drive-map info terms env)
  (map (cut drive info <> env) terms))

(define (drive info term env)
  (let-syntax
      ((drive-distribute
        (syntax-rules ()
          ((_ (predicate proc) ...)
           (cond ((predicate term) (proc info term env)) ...)))))

    (drive-distribute
     ;(sopt-if?      drive-if)
     ;(sopt-apply?   drive-apply)
     ;(sopt-lambda?  drive-lambda)
     ;(sopt-call/cc? drive-call/cc)
     ;(sopt-set!     drive-set!)
     ;(sopt-call?    drive-call)
     (sopt-let?     drive-let)
     (sopt-var?     drive-var)
     (sopt-literal? drive-literal))))

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
           (drive-map info (sopt-let-terms term) (reverse new-env)))

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
