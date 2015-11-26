(define-module sopt.io
  (use sopt.data)
  (use util.match)
  (export
   sopt-read
   sopt-write))
(select-module sopt.io)


(define (src->record s)
  (define src-pat->record
    (match-lambda
      [() '()]

      [(ca . cd) (cons (make-var ca) (make-var cd))]
      ))

  (define src-binding->record
    (match-lambda
     [(v t) (cons (src->record v) (src->record t))]))

  (match s
   [('define (name . args) . body)
    (make-fun name (map src->record args) (map src->record body))]

   [('if t1 t2 t3)
    (make-ifs (src->record t1)
              (src->record t2)
              (src->record t3))]

   [('case t0 . clauses)
    (make-cas (src->record t0)
              (map (^[c] (cons (src-pat->record (car c)) (src->record (cadr c))))
                   clauses))]

   [('quote lst) lst]

   [('let bindings . body)
    (make-les (map src-binding->record bindings)
              (map src->record body))]

   [(fun . args)
    (make-app (car s) (map src->record (cdr s)))]

   [else
    (if (symbol? s)
        (make-var s)
        s)]
   ))

(define (record->src p)
  (define record-pat->src
    (match-lambda
      [() '()]

      [(ca . cd) (cons (var-name ca) (var-name cd))]
      ))

  (define record-binding->src
    (match-lambda
     [(v . t) (list (record->src v) (record->src t))]))

  (cond
   [(fun? p)
    `(define (,(fun-name p) . ,(map record->src (fun-args p)))
       . ,(map record->src (fun-body p)))]

   [(ifs? p)
    `(if ,(record->src (ifs-test p))
         ,(record->src (ifs-then p)) ,(record->src (ifs-else p)))]

   [(cas? p)
    `(case ,(record->src (cas-key p))
       .
       ,(map
         (^[c]
           (list (record-pat->src (car c)) (record->src (cdr c))))
         (cas-clauses p)))]

   [(les? p)
    `(let ,(map record-binding->src (les-bindings p))
       .  ,(map record->src (les-body p)))]

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

(define (sopt-read :optional (iport (current-input-port)))
  (rlet1 funs '()
    (until (read iport) eof-object? => s
      (push! funs (src->record s))
      )))

(define (sopt-write funs :optional (oport (current-output-port)))
  (for-each
   (^[x]
     (write (record->src x) oport)
     (newline oport))
   funs))
