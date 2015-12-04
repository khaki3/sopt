(define-module sopt
  (use sopt.data)
  (use sopt.eval)
  (export
   string->sopt-args
   make-sopt-args
   SOPT_UNDEF
   port->sopt-cxt
   write-sopt-cxt
   sopt-eval
   ))
