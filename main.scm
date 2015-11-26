(add-load-path "." :relative)
(use sopt)

(define (main args)
  (let* ([iport (if (= (length args) 2)
                    (open-input-file (~ args 1))
                    (current-input-port))]
         [funs  (call-with-port iport sopt-read)])
    (sopt-write (sopt-run funs))
    ))
