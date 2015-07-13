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
(use gauche.pp)
(use util.match)

(define-record-type fun #t #t
  name args body)

(define-record-type app #t #t
  fun-name args)

(define-record-type cas #t #t
  key clauses)

(define-record-type ifs #t #t
  testl testr th el)

(define (src->record s)
  (define (app->record s)
    (make-app (car s) (map src->record (cdr s))))

  (match s
   [('def (name . args) body)
    (make-fun name args (src->record body))]

   [('if ('= t1 t2) t3 t4)
    (make-ifs (src->record t1)
              (src->record t2)
              (src->record t3)
              (src->record t4))]

   [('case t0 . clauses)
    (make-cas (src->record t0)
              (map (^[c] (cons (car c) (src->record (cadr c))))
                   clauses))]

   [('quote lst) lst]

   [(fun . args)
    (app->record s)]

   [else s]
   ))

(define (record->src p)
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
           (list (car c) (record->src (cdr c))))
         (cas-clauses p)))]

   [(app? p)
    `(,(app-fun-name p) . ,(map record->src (app-args p)))]

   [(pair? p)
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

(define (ps funs)
  (let ([bindings (make-hash-table 'equal?)]
        [specials (make-hash-table 'equal?)])
    (define (value? t)
      (not (or (record? t) (symbol? t))))

    (define (closed? t)
      (if (pair? t) (closed-pair? t)
          (value? t)))

    (define (closed-pair? t)
      (define (value-inner-pair? t)
        (not (or (record? t)
                 (and (symbol? t)
                      (eq? t 'undef)))))

      (if (pair? t) (and (closed-pair? (car t)) (closed-pair? (cdr t)))
          (value-inner-pair? t)))

    ;; env ::= '((sym . parameter) ...)
    (define (drive t env)

      (define (drive-all lst)
        (map (cut drive <> env) lst))

      (define (ref-param v)
        (assoc-ref env v))

      (define (deparam-pair pr)
        (cons (deparam (car pr)) (deparam (cdr pr))))
      
      (define (deparam p)
        (if (not (parameter? p)) p
            (let ([d (p)])
              (if (pair? d) (deparam-pair d) d))))

      (define (ref v)
        (or (and-let* ([p (ref-param v)]
                       [result (deparam p)])
              (and (not (eq? result 'undef))
                   result))
            v
            ))

      (cond
       [(value? t) t]

       [(not (record? t))
        (ref t)]

       [(app? t)
        (let* ([aname (app-fun-name t)]
               [aargs (app-args t)]
               [drived-args (drive-all aargs)]
               [normalized (map (^[x] (if (value? x) x 'undef)) drived-args)]
               [new-aname (bind! aname normalized)])

          (cond [new-aname (make-app new-aname (remove closed? drived-args))]

                [(and (global-variable-bound? (current-module) 'car)
                      (every (^[x] (and (not (pair? x)) (closed? x))) drived-args))
                 (eval (cons aname drived-args) (current-module))]

                [else (make-app aname drived-args)])
          )]
              

       [(ifs? t)
        (let* ([orig-testl (ifs-testl t)]
               [orig-testr (ifs-testr t)]
               [testl (drive orig-testl env)]
               [testr (drive orig-testr env)])
          (if (and (closed? testl) (closed? testr))
              (drive ((if (equal? testl testr) ifs-th ifs-el) t) env)

              (make-ifs testl testr
                (cond [(and (closed? testl) (symbol? orig-testr) (ref-param orig-testr))
                       => (^[p]
                            (parameterize ((p testl))
                              (drive (ifs-th t) env)))]

                      [(and (closed? testr) (symbol? orig-testl) (ref-param orig-testl))
                       => (^[p]
                            (parameterize ((p testr))
                              (drive (ifs-th t) env)))]

                      [else (drive (ifs-th t) env)])

                (drive (ifs-el t) env))
              ))]

       [(cas? t)
        (let ([key     (drive (cas-key t) env)]
              [clauses (cas-clauses t)])
          (define (pat-match? pat t)
            (match pat
              [() (null? t)]

              [(ca . cd) (pair? t)]
              ))

          (define (new-env pat t :optional (env '()))
            (match pat
              [() env]

              [(ca . cd)
               (if (value? t)
                   `(,(cons ca (make-parameter (car t)))
                     ,(cons cd (make-parameter (cdr t)))
                     . ,env)
               
                   (let ([t ((ref-param t))])
                     `(,(cons ca (car t)) ,(cons cd (cdr t)) . ,env)
                     ))]
              ))

          (define (gen-value pat)
            (match pat
              [() ()]

              [(ca . cd) (cons (make-parameter 'undef) (make-parameter 'undef))]
              ))

          (cond [(value? key)
                 (let ([c (find (^[c] (pat-match? (car c) key)) clauses)])
                   (drive (cdr c) (new-env (car c) key env)))]

                [(symbol? key)
                 (let ([p (ref-param key)])
                   (make-cas key
                     (map (^[c]
                            (let ([pat (car c)]
                                  [exp (cdr c)])
                              (cons pat
                                    (parameterize ([p (gen-value pat)])
                                      (drive exp (new-env pat key env))))
                              ))
                          clauses)
                     ))]

                [else
                 (make-cas key
                   (map (^[c] (cons (car c) (drive (cdr c) env))) clauses))]

                ))]
       ))

    (define (bind! fname args)
      (let ([bkey (cons fname args)])

        (or ;; already binded
            (hash-table-get bindings bkey #f)

            ;; a new binding to user-defined function
            (and-let1 f (find-fun fname)
              (let* ([fa (fun-args f)]
                     [fb (fun-body f)]
                     [new-name
                      (if (any value? args) (gensym (symbol-append fname '--)) fname)])

                (define (construct-parameter t)
                  (if (pair? t)
                      (make-parameter
                       (cons (construct-parameter (car t))
                             (construct-parameter (cdr t))))

                      (make-parameter t)))

                (hash-table-put! bindings bkey new-name)
                (hash-table-put! specials
                  new-name
                  (drive fb (map (^[v t] (cons v (construct-parameter t))) fa args)))

                new-name
                ))
            )))

    (define (find-fun fname)
      (find (^[f] (eq? (fun-name f) fname)) funs))

    ;; specialize main-function
    (bind! 'main '(undef))

    ;; arrange specialized-functions
    (hash-table-map bindings
      (^[applying sname]

        (let ([spec (hash-table-get specials sname)]
              [new-args
               (let* ([orig-name (car applying)]
                      [args      (cdr applying)]
                      [fa  (fun-args (find-fun orig-name))])
                 (fold (^[v t lst] (if (closed? t) lst (cons v lst))) '() fa args))])

          (make-fun sname new-args spec))

        ))))

(define (print-fun fun)
  (pprint (record->src fun))
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
