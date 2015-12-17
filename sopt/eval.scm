(define-module sopt.eval
  (use sopt.data)
  (use sopt.ext)
  (use util.match)
  (use gauche.record)
  (use gauche.parameter)
  (use srfi-11)
  (export sopt-eval))
(select-module sopt.eval)

(define-record-type sopt-info %make-sopt-info #f
  cxt
  ext
  bind   ; hashtable((name, args) -> new-name)
  opt    ; hashtable(name -> optimized-def)
  remain ; hashtable(name -> boolean)          # whether it must remain on source-code.
  )

(define (make-sopt-info cxt ext)
  (%make-sopt-info
   cxt
   ext
   (make-hash-table 'equal?)
   (make-hash-table 'eq?)
   (make-hash-table 'eq?)))

(define (info->cxt info)
  (make-sopt-cxt
   (alist->hash-table
    (hash-table-map (sopt-info-remain info)
      (lambda (name _)
        (cons name (info-opt-ref info name)))))))

(define (info-bind! info name args new-name)
  (hash-table-put! (sopt-info-bind info) (cons name args) new-name))

(define (info-bind-ref info name args)
  (ref (sopt-info-bind info) (cons name args) #f))

(define (info-add-opt! info name def)
  (hash-table-put! (sopt-info-opt info) name def))

(define (info-opt-ref info name)
  (ref (sopt-info-opt info) name #f))

(define (info-remain! info name)
  (hash-table-put! (sopt-info-remain info) name #t))

(define (info-cxt-ref info name)
  (sopt-cxt-ref (sopt-info-cxt info) name))

#|
   target-args   [from command-line]
     SOPT_UNDEF, var, literal

   template-args [arguments of definition]
     var

   passed-args   [sopt-call-args]
     var, literal, lambda, sopt-*

   native-args   [run-time args]
     var, literal, lambda

   plain-args    [for hash-table's key]
     SOPT_UNDEF, literal-value
|#

;; valid for native-args?
(define (native-data? x)
  (or (sopt-var? x) (sopt-literal? x) (sopt-lambda? x)))

;; target-args -> native-args
;; replace undef by template-args
(define (enable-args info target target-args)
  (and-let1 target-def (sopt-cxt-ref (sopt-info-cxt info) target)
    (map
     (lambda (templ targ)
       (if (undef? targ) templ targ))
     (sopt-def-args target-def) target-args)))

;; template-args -> passed-args -> native-args
(define (normalize-args template-args passed-args)
  (map
   (lambda (t p) (if (native-data? p) p t))
   template-args passed-args))

;; native-args or passed-args -> plain-args
(define (generalize-args native-args)
  (map
   (lambda (n)
     (cond [(sopt-literal? n) (sopt-literal-value n)]
           [(sopt-lambda?  n) (sopt-lambda-data   n)]
           [(sopt-var?     n) SOPT_UNDEF]
           [else (error "Invalid native-args: ~native-args")]))
   native-args))

(define (sopt-eval cxt ext target target-args)
  (or (and-let* ([info (make-sopt-info cxt ext)]
                 [def  (sopt-opt! info target (enable-args info target target-args))])
        (info-remain! info (sopt-def-name def))
        (info->cxt info))
      cxt))

;; delete the native-data without var
(define (reduce-args args)
  (filter-map
   (lambda (p) (cond [(sopt-var? p) p] [(native-data? p) #f] [else p]))
   args))

(define-method make-sopt-env (template-args native-args)
  (map
   (lambda (t n)
     (cons t (if (sopt-var? n)
                 (make-sopt-trace n SOPT_UNDEF)
                 (make-sopt-trace t n))))
   template-args native-args))

(define (sopt-opt! info name native-args)
  (and-let1 def (info-cxt-ref info name)
     (let* ([plain-args    (generalize-args native-args)]
            [origin        (every undef? plain-args)]
            [new-name      (if origin name (sopt-gensym name))]
            [template-args (sopt-def-args def)]
            [env           (make-sopt-env template-args native-args)])
       (info-bind! info name plain-args new-name)

       (rlet1 new-def
          (make-sopt-def
           new-name
           (reduce-args native-args)
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
     (sopt-if?      drive-if)
     (sopt-lambda?  drive-lambda)
     (sopt-call/cc? drive-call/cc)
     (sopt-call?    drive-call)
     (sopt-apply?   drive-apply)
     (sopt-let?     drive-let)
     (sopt-var?     drive-var)
     (sopt-literal? drive-literal))))

(define (ps-fetch env t1 t2)
  (cond [(and (sopt-var? t1) (sopt-literal? t2) (sopt-env-ref env t1))
         => (lambda (trace) (values trace t2))]

        [(and (sopt-var? t2) (sopt-literal? t1))
         (ps-fetch env t2 t1)]

        [else (values #f #f)]))

(define-syntax ps
  (syntax-rules ()
    [(_ env testr testl body ...)
     (let-values ([(trace literal) (ps-fetch env testr testl)])
       (if trace
           (parameterize ([(cdr trace) literal]) body ...)
           (begin body ...)))]))

(define (drive-if info term env)
  (let* ([if-test (drive info (sopt-if-test term) env)]
         [if-then (sopt-if-then term)]
         [if-else (sopt-if-else term)])

    (if (sopt-literal? if-test)
        (if (sopt-literal-value if-test)
            (drive info if-then env)
            (drive info if-else env))

        (or (and (sopt-call? if-test)
                 (eq? (sopt-call-proc if-test) 'equal?)

                 ;; positive supercompiling
                 (let* ([call-args (sopt-call-args if-test)]
                        [testl     (~ call-args 0)]
                        [testr     (~ call-args 1)])
                   (or (and (sopt-literal? testl)
                            (sopt-literal? testr)
                            (if (equal? (sopt-literal-value testl)
                                        (sopt-literal-value testr))
                                (drive info if-then env)
                                (drive info if-else env)))

                       (make-sopt-if
                        if-test
                        (ps env testr testl
                            (drive info if-then env))
                        (drive info if-else env)))))

            (make-sopt-if
             if-test
             (drive info if-then env)
             (drive info if-else env))))))

(define (drive-lambda info term env)
  (let* ([lmd-args  (sopt-lambda-args  term)]
         [lmd-terms (sopt-lambda-terms term)]
         [env
          (let loop ([la lmd-args] [env env])
            (if (null? la) env
                (loop (cdr la)
                      (add-trace env (car la)
                        (make-sopt-trace (car la) SOPT_UNDEF)))))])
    (make-sopt-lambda lmd-args (drive-map info lmd-terms env))))

(define (drive-call/cc info term env)
  (make-sopt-call/cc (drive info (sopt-call/cc-proc term) env)))

;; template-args -> passed-args -> bindings
(define (caller-bindings template-args passed-args)
  (filter-map
   (lambda (t p) (if (native-data? p) #f (cons t p)))
   template-args passed-args))

(define (construct-let bindings terms)
  (if (and (null? bindings) (= (length terms) 1))
      (car terms)
      (make-sopt-let bindings terms)))

(define (drive-call info term env)
  (let* ([proc        (drive info (sopt-call-proc term) env)]
         [passed-args (drive-map info (sopt-call-args term) env)]
         [plain-args  (generalize-args passed-args)])

    (or ;; env search
        (and (sopt-lambda? proc)
             (let* ([template-args (sopt-lambda-args proc)]
                    [native-args   (normalize-args  template-args passed-args)]
                    [bindings      (caller-bindings template-args passed-args)]
                    [env           (append (make-sopt-env template-args native-args) env)]
                    [lmd-terms     (drive-map info (sopt-lambda-terms proc) env)])
               (construct-let bindings lmd-terms)))

        (and (sopt-var? proc)
             (or ;; cxt search
                 (and-let1 def (info-cxt-ref info proc)
                    (if-let1 name (info-bind-ref info proc plain-args)
                       ;; already optimized
                       (begin
                         (info-remain! info name)
                         (make-sopt-call name (reduce-args passed-args)))

                       ;; first optimizing
                       (let* ([template-args (sopt-def-args def)]
                              [native-args   (normalize-args  template-args passed-args)]
                              [bindings      (caller-bindings template-args passed-args)]
                              [new-def       (sopt-opt! info proc native-args)]
                              [def-terms     (sopt-def-terms new-def)])
                         (construct-let bindings def-terms))))

                 ;; ext search
                 (and-let1 ext (and (sopt-info-ext info)
                                    (every sopt-literal? passed-args)
                                    (sopt-ext-ref proc))
                    (make-sopt-literal (apply ext plain-args)))
                 ))

        (make-sopt-call proc passed-args))))

(define (drive-apply info term env)
  (let* ([proc       (drive info (sopt-apply-proc term) env)]
         [list-term  (drive info (sopt-apply-list term) env)]
         [list-value (and (sopt-literal? list-term) (sopt-literal-value list-term))])

    (if (and list-value (list? list-value))
        (drive info (make-sopt-call  proc (map make-sopt-literal list-value)) env)
        (make-sopt-apply proc list-term))))

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
          (construct-let new-bindings (drive-map info (sopt-let-terms term) new-env))

          (let* ([b-var  (caar bindings)]
                 [b-term (drive info (cdar bindings) env)]
                 [disappearable-var
                  (and (sopt-var? b-term) (not (memq b-term declared)))]

                 [new-env
                  (add-trace new-env b-var
                    (cond [disappearable-var
                           (or (sopt-env-ref env b-term)
                               (make-sopt-trace b-term SOPT_UNDEF))]

                          [(or (sopt-literal? b-term) (sopt-lambda? b-term))
                           (make-sopt-trace b-var b-term)]

                          [else
                           (make-sopt-trace b-var SOPT_UNDEF)]))]

                 [new-bindings
                  (if (or disappearable-var (sopt-literal? b-term) (sopt-lambda? b-term))
                      new-bindings (cons (cons b-var b-term) new-bindings))])

            (loop (cdr bindings) new-bindings new-env))))))

(define (drive-var info term env)
  (if-let1 trace (sopt-env-ref env term)
     (let ([var ((car trace))]
           [val ((cdr trace))])
       (if (undef? val) var val))
     term))

(define (drive-literal info term env) term)
