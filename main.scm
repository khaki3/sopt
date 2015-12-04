(add-load-path "." :relative)
(use sopt)
(use gauche.parseopt)

(define (usage cmd)
  (display
#"
                 _   
 ___  ___  _ __ | |_ 
/ __|/ _ \\| '_ \\| __|
\\__ \\ (_) | |_) | |_ 
|___/\\___/| .__/ \\__|
          |_|

Usage: ~cmd [--in=file] [--out=file] [-e|--ext] [--fun=name] [--args=args]

" (current-error-port)))

(define (main args)
  (let-args (cdr args)
      ((help  "h|help")
       (in    "i|in=s")
       (out   "o|out=s")
       (ext   "e|ext")     ; extend cxt by R5RS-functions without IO
       (sfun  "f|fun=s")   ; a function will be optimized
       (sargs "a|args=s")) ; args passed to the function above

    (when help
      (usage (car args))
      (exit))

    (let* ([iport (if in    (open-input-file   in)    (current-input-port))]
           [oport (if out   (open-output-file  out)   (current-output-port))]
           [fun   (if sfun  (string->symbol    sfun)  'main)]
           [args  (if sargs (string->sopt-args sargs) (make-sopt-args SOPT_UNDEF))]
           [cxt           (port->sopt-cxt iport)]
           [optimized-cxt (sopt-eval cxt ext fun args)])

      (write-sopt-cxt optimized-cxt oport)
      (close-port iport)
      (close-port oport)
      )))
