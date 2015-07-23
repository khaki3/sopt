;;;
;;; Positive Supercompiler + Online partial evaluator
;;; (Implemented by Gauche)
;;;
;;; References:
;;;  [1] M.H. Sørensen, R. Gluck, and N.D. Jones. 1993. A Positive Supercompiler.
;;;

;;
;; q ::= d1 .. dM
;;
;; d ::= (def (f v1 .. vN) t)
;;
;; t ::= value
;;     | v                     [variable]
;;     | (f t1 .. tN)          [function call]
;;     | (case t0 (p1 t1) ..)  [case-expression]
;;     | (if (= t1 t2) t3 t4)  [conditional]
;;
;; p ::= (ca . cd) | ()
;;

(use gauche.parameter)
(use gauche.record)
(use util.match)

(define-record-type fun #t #t
  name args body)

(define-record-type app #t #t
  fun-name args)

(define-record-type cas #t #t
  key clauses)

(define-record-type ifs #t #t
  testl testr th el)

(define-record-type var #t #t
  name)

(define-method object-equal? ((a var) (b var))
  (eq? (var-name a) (var-name b)))

(define (src->record s)
  (define (srcpat->record sp)
    (match sp
      [() '()]

      [(ca . cd) (cons (make-var ca) (make-var cd))]
      ))

  (match s
   [('def (name . args) body)
    (make-fun name (map src->record args) (src->record body))]

   [('if ('= t1 t2) t3 t4)
    (make-ifs (src->record t1)
              (src->record t2)
              (src->record t3)
              (src->record t4))]

   [('case t0 . clauses)
    (make-cas (src->record t0)
              (map (^[c] (cons (srcpat->record (car c)) (src->record (cadr c))))
                   clauses))]

   [('quote lst) lst]

   [(fun . args)
    (make-app (car s) (map src->record (cdr s)))]

   [else
    (if (symbol? s)
        (make-var s)
        s)]
   ))

(define (record->src p)
  (define (recordpat->src rp)
    (match rp
      [() '()]

      [(ca . cd) (cons (var-name ca) (var-name cd))]
      ))

  (cond
   [(fun? p)
    `(def (,(fun-name p) . ,(map record->src (fun-args p)))
          ,(record->src (fun-body p)))]

   [(ifs? p)
    `(if (= ,(record->src (ifs-testl p)) ,(record->src (ifs-testr p)))
         ,(record->src (ifs-th p)) ,(record->src (ifs-el p)))]

   [(cas? p)
    `(case ,(record->src (cas-key p))
       .
       ,(map
         (^[c]
           (list (recordpat->src (car c)) (record->src (cdr c))))
         (cas-clauses p)))]

   [(pair? p)
    `(quote ,p)]

   [(app? p)
    `(,(app-fun-name p) . ,(map record->src (app-args p)))]

   [(var? p)
    (var-name p)]

   [(symbol? p)
    `(quote ,p)]

   [else p]
   ))

(define (parameter? obj)
  (is-a? obj <parameter>))

(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1) (symbol->string s2))))

(define GENSYM-COUNT 0)

;; readable gensym
(define (gensym :optional (prefix 'f--))
  (inc! GENSYM-COUNT)
  (symbol-append prefix (string->symbol (number->string GENSYM-COUNT))))

(define-syntax save-list
  (syntax-rules ()
    [(_ lst body ...)
     (let* ([save   (list-copy lst)]
            [result (begin body ...)])
       (set! lst save)
       result)]))

(define-constant UNDEF 'ps--UNDEF)

;;;
;;; Positive-supercompile funs
;;;
(define (ps funs)
  (let ([bindings (make-hash-table 'equal?)]
        [specials (make-hash-table 'eq?)])

    (define (find-fun fname)
      (find (^[f] (eq? (fun-name f) fname)) funs))

    (define (value? t)
      (not (record? t)))

    (define (closed-pair? pair)
      (unless (pair? pair) (error "closed-pair?"))

      (and (closed? (car pair)) (closed? (cdr pair))))

    (define (closed? val)
      (and (value? val)
           (not (eq? val UNDEF))
           (or (not (pair? val))
               (closed-pair? val))
           ))

    ;;
    ;; Positive-supercompile a expression t
    ;;
    ;;   env ::= '((var . parameter) ...)
    ;;
    (define (drive t env)

      (define (ref-param env v)
        (unless (var? v) (error "ref-param"))
        (assoc-ref env v))

      (define (deparam p)
        (define (deparam-pair pair)
          (unless (pair? pair) (error "deparam-pair"))
          (let ([ca (car pair)]
                [cd (cdr pair)])
            (cons (if (parameter? ca) (deparam ca) ca)
                  (if (parameter? cd) (deparam cd) cd)
                  )))

        (unless (parameter? p) (error "deparam"))
        (let ([d (p)])
          (if (pair? d) (deparam-pair d) d)))


      ;; trace the original name of the var
      (define (trace-var v)
        (unless (var? v) (error "trace-var"))

        (let loop ([v v] [lst TRACING])
          (cond [(null? lst) v]

                [(equal? (caar lst) v)

                 (let1 cd (cdar lst)
                   (if (equal? cd v) v
                       (loop cd (cdr lst))
                       ))]

                [else (loop v (cdr lst))]
                )))

      ;;
      ;; v1 is the child of v2
      ;;   v1 -> v2
      ;;
      (define (connect-var! v1 v2)
        (push! TRACING (cons v1 v2)))

      ;;
      ;; '((child . parent) ...)
      ;;
      ;;   It doesn't become a loop.
      ;;
      ;;   TRACING = '((var-x . var-y) (var-y . var-x))
      ;;   (trace-var var-x) => var-y
      ;;
      ;;
      ;;   Self-references mean stop.
      ;;
      ;;   TRACING = '((var-x var-y) (var-y . var-y) (var-y . var-z))
      ;;   (trace-var var-x) => var-y
      ;;
      (define TRACING '())

      ;;
      ;; If t is a variable which contains a closed(sourceable)-value,
      ;; this function replaces it by the value.
      ;;
      ;;   Be careful of variables that contain #f
      ;;
      (define (formalize t env)
        (if (not (var? t)) t
            (let* ([p   (ref-param env t)]
                   [val (and p (deparam p))])
              (if (and p (closed? val)) val
                  (trace-var t)
                  ))
            ))

      ;;
      ;; Inner drive for sharing of TRACING
      ;;
      (define (drive t env)
        (formalize (execute t env) env))

      ;;
      ;; driving-core
      ;;
      (define (execute t env)

        ;;
        ;; If t is a variable, this function replaces it by the value.
        ;;
        (define (valueize t)
          (let* ([p   (and (var? t) (ref-param env t))]
                 [val (and p (deparam p))])
            (if p val t)))

        ;; remove incomplete-informations
        ;;   propagate "value"s only, and translate others as UNDEF
        (define (remove-incompletes x)

          (define ri remove-incompletes)

          ;; (UNDEF . UNDEF) -> 'UNDEF
          (define (ri-pair x)
            (unless (pair? x) (error "ri-pair"))

            (if (and (eq? (car x) UNDEF)
                     (eq? (cdr x) UNDEF)) UNDEF
                (cons (ri (car x)) (ri (cdr x)))
                ))

          (cond [(not (value? x)) UNDEF]
                [(pair? x) (ri-pair x)]
                [else x]))

        (define (map-with-env f args)
          (map (cut f <> env) args))


        (cond
         [(value? t) t]

         [(var? t) t]

         [(app? t)
          (let* ([aname (app-fun-name t)]
                 [aargs (app-args t)]

                 [executed-args (map-with-env execute aargs)]
                 [passing-args (map (compose remove-incompletes valueize) executed-args)]
                 [formalized-args (map-with-env formalize executed-args)]

                 [bkey (cons aname passing-args)]
                 [never-folded (not (hash-table-exists? bindings bkey))]
                 [f (find-fun aname)]
                 [fa (and f (fun-args f))]
                 [fb (and f (fun-body f))]

                 [new-aname (bind-name! aname passing-args)])

            (define (new-env)
              (append

               (map
                (^[v t]
                  (cond [(value? t)
                         (cons v (make-parameter t))]

                        [(and (var? t) (ref-param env t))
                         => (^[p] (cons v p))]

                        [else
                         (cons v (make-parameter UNDEF))]
                        ))
                fa executed-args)

               env))

            (cond [(and new-aname never-folded)
                   (save-list TRACING
                    (for-each (^[v t] (when (var? t) (connect-var! v t))) fa executed-args)
                    (drive fb (new-env)))]

                  [new-aname
                   (unless (hash-table-exists? specials new-aname)
                     (bind-specialize! aname passing-args))
                   (make-app new-aname (remove closed? formalized-args))]

                  ;; partial evaluation
                  [(and (global-variable-bound? (current-module) aname)
                        (every closed? formalized-args))
                   (eval (cons aname formalized-args) (current-module))]

                  [else (make-app aname formalized-args)])
            )]

         [(ifs? t)
          (let* ([orig-testl (ifs-testl t)]
                 [orig-testr (ifs-testr t)]
                 [executed-testl   (execute orig-testl env)]
                 [executed-testr   (execute orig-testr env)]
                 [valueized-testl  (valueize executed-testl)]
                 [valueized-testr  (valueize executed-testr)]
                 [formalized-testl (formalize executed-testl env)]
                 [formalized-testr (formalize executed-testr env)])

            (if (and (closed? valueized-testl) (closed? valueized-testr))
                (drive ((if (equal? valueized-testl valueized-testr) ifs-th ifs-el) t) env)

                (make-ifs formalized-testl formalized-testr
                  (let-syntax
                      ([prop
                        (syntax-rules ()
                          [(_ src dest)
                           (let* ([p (ref-param env src)]
                                  [undefined (not p)]
                                  [p   (if undefined (make-parameter UNDEF) p)]
                                  [env (if undefined `(,(cons src p) . ,env) env)])
                             (parameterize ((p dest))
                               (drive (ifs-th t) env))
                             )])])

                    (cond [(and (var? executed-testl) (closed? valueized-testr))
                           (prop executed-testl valueized-testr)]

                          [(and (var? executed-testr) (closed? valueized-testl))
                           (prop executed-testr valueized-testl)]

                          [else (drive (ifs-th t) env)]))

                  (drive (ifs-el t) env))
                ))]

         [(cas? t)
          (let* ([key            (cas-key t)]
                 [clauses        (cas-clauses t)]
                 [executed-key   (execute key env)]
                 [valueized-key  (valueize  executed-key)]
                 [formalized-key (formalize executed-key env)])

            (define (pat-match? pat t)
              (match pat
                [() (null? t)]

                [(ca . cd) (pair? t)]
                ))

            (define (new-env pat t :optional (env env))
              (match pat
                [() env]

                [(ca . cd)
                 (cond [(value? t)
                        `(,(cons ca (make-parameter (car t)))
                          ,(cons cd (make-parameter (cdr t)))
                          . ,env)]

                       [(and (var? t) (ref-param env t))
                        =>
                        (^[p]
                          (let ([t (p)])
                            `(,(cons ca (car t)) ,(cons cd (cdr t)) . ,env)
                            ))]
                       )]
                ))

            (define (gen-value pat)
              (match pat
                [() ()]

                [(ca . cd) (cons (make-parameter UNDEF) (make-parameter UNDEF))]
                ))

            (define (pat-connect! pat)
              (match pat
                [() ()]

                [(ca . cd) (connect-var! ca ca) (connect-var! cd cd)]
                ))

            (cond [(and (value? valueized-key)
                        (find (^[c] (pat-match? (car c) valueized-key)) clauses))
                   =>
                   (^[c]
                     (drive (cdr c) (new-env (car c) valueized-key)))]

                  [(var? executed-key)
                   (let* ([p (ref-param env executed-key)]
                          [undefined (not p)]
                          [p   (if undefined (make-parameter UNDEF) p)]
                          [env (if undefined `(,(cons executed-key p) . ,env) env)])
                     (make-cas formalized-key
                       (map (^[c]
                              (let ([pat (car c)]
                                    [exp (cdr c)])
                                (save-list TRACING
                                  (pat-connect! pat)
                                  (cons pat
                                        (parameterize ([p (gen-value pat)])
                                          (drive exp (new-env pat executed-key env)))))
                                ))
                            clauses)
                       ))]

                  [else
                   (make-cas formalized-key
                     (map (^[c] (cons (car c) (drive (cdr c) env))) clauses))]

                  ))])) ;; execute

      (drive t env)) ;; drive


    (define (bind-name! fname args)
      (let ([bkey (cons fname args)])

        (or ;; already binded
            (hash-table-get bindings bkey #f)

            ;; a new binding to user-defined function
            (and (find-fun fname)
                 (rlet1 new-name (if (every (cut eq? UNDEF <>) args) fname
                                     (gensym (symbol-append fname '--)))
                   (hash-table-put! bindings bkey new-name)
                   ))
            )))

    (define (bind-specialize! fname args)
      (define (construct-parameter t)
        (if (pair? t)
            (make-parameter
             (cons (construct-parameter (car t))
                   (construct-parameter (cdr t))))

            (make-parameter t)))

      (let* ([bkey (cons fname args)]
             [new-name (hash-table-get bindings bkey)]

             [f (find-fun fname)]
             [fa (fun-args f)]
             [fb (fun-body f)])

        (hash-table-put! specials new-name #f) ; temporary
        (hash-table-put! specials new-name
          (drive fb (map (^[v t] (cons v (construct-parameter t))) fa args)))
        ))

    (define (bind! fname args)
      (let ([bkey (cons fname args)])

        (or ;; already binded
            (hash-table-get bindings bkey #f)

            ;; a new binding to user-defined function
            (rlet1 new-name (bind-name! fname args)
              (and new-name (bind-specialize! fname args))))
        ))

    ;; specialize main-function
    (bind! 'main (list UNDEF))

    ;; arrange specialized-functions
    (filter values
     (hash-table-map bindings
      (^[applying sname]

        (and (hash-table-exists? specials sname) ; Be careful when spec is #f
             (let ([spec (hash-table-get specials sname)]
                   [new-args
                    (let* ([orig-name (car applying)]
                           [args      (cdr applying)]
                           [fa  (fun-args (find-fun orig-name))])
                      (filter-map (^[v t] (and (not (closed? t)) v)) fa args))])

               (make-fun sname new-args spec))

             ))))
    )) ;; ps

(define (print-fun fun)
  (write (record->src fun))
  (newline)
  (newline))

(define (ps-test funs)
  (for-each print-fun (ps funs)))

(define (ps-reader iport)
  (rlet1 funs '()
    (until (read iport) eof-object? => s
      (push! funs (src->record s))
      )))

(define (main args)
  (ps-test
   (if (= (length args) 2)
       (call-with-input-file (~ args 1) ps-reader)
       (ps-reader (current-input-port)))
   ))
