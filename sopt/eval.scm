(define-module sopt.eval
  (use sopt.data)
  (use sopt.ext)
  (use util.match)
  (use gauche.record)
  (use gauche.parameter)
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
  (hash-table-put! (hash-table-bind info) (cons name args) new-name))

(define (info-add-opt! info name def)
  (hash-table-put! (sopt-info-opt info) name def))

(define (sopt-eval cxt ext target target-args)
  (let1 info (make-sopt-info cxt ext)
    (sopt-opt! info target target-args)
    (info->cxt info)))

(define (reduce-args template-args actual-args)
  (filter-map
    (lambda (t a) (and (not (undef? a)) t))
    template-args actual-args))

(define (sopt-opt! info target target-args)
  (and-let1 target-def (sopt-cxt-ref (sopt-info-cxt info) target)
    (let* ([origin (every undef? target-args)]
           [name   (if origin target (sopt-gensym target))]
           [env    #f]) ; todo
      (unless origin
        (info-bind! info target target-args name))

      (info-add-opt! info name
        (make-sopt-def
          name
          (reduce-args (sopt-def-args target-def) target-args)
          (map (cut drive info <> env) (sopt-def-terms target-def))
          )))))

(define (drive info term env)
  ; todo
  )
