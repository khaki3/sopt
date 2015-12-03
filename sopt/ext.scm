;; sopt extension module
(define-module sopt.ext
  (export sopt-ext-ref))

(define-constant SCMMOD (find-module 'scheme))

(define-syntax as-alist
  (syntax-rules ()
    [(_ key ...)
     (quote ((key . #t) ...))]))

(define-constant IO_ALIST
  (as-alist
    open-input-file
    open-output-file
    close-input-port
    close-output-port
    current-input-port
    current-output-port
    with-input-from-file
    with-output-to-file
    call-with-output-file
    call-with-input-file
    peek-char
    write-char
    newline
    read
    read-char
    write
    display
    ))
  
(define-constant IO_HT
  (alist->hash-table IO_ALIST))

(define (io? symbol)
  (ref IO_HT symbol #f))

(define (sopt-ext-ref symbol)
  (and (not (io? symbol)) (global-variable-ref SCMMOD symbol #f)))
