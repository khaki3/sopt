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

(define (drive info term env)
  (cond
   [(sopt-var? term)
    (if-let1 trace (sopt-env-ref env term)
       (let ([var ((car trace))]
             [val ((cdr trace))])
         (if (undef? val) var val))
       term)]

   [(sopt-literal? term) term]))
