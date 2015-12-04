(add-load-path "." :relative)
(use sopt)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args)
      ((in    "i|in=s")
       (out   "o|out=s")
       (ext   "e|ext")     ; extend cxt by R5RS-functions without IO
       (sfun  "f|fun=s")   ; a function will be optimized
       (sargs "a|args=s")) ; args passed to the function above

    (let* ([iport (if in    (open-input-file   in)    (current-input-port))]
           [oport (if out   (open-output-file  out)   (current-output-port))]
           [fun   (if sfun  (string->symbol    sfun)  'main)]
           [args  (if sargs (string->sopt-args sargs) (make-sopt-args SOPT_UNDEF))]
           [cxt           (port->sopt-cxt iport)]
           [optimized-cxt (sopt-eval cxt fun args ext)])

      (write-sopt-cxt optimized-cxt oport)
      (close-port iport)
      (close-port oport)
      )))
