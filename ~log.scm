% cat ../test/kmp-test.scm
(define (match p s) (loop p s p s))

(define (loop pp ss op os)
  (case pp
    (() #t)
    ((p . pp)
     (case ss
       (() #f)
       ((s . ss)
        (if (equal? p s)
            (loop pp ss op os)
            (next op os)
            ))))))

(define (next op os)
  (case os
    (() #f)
    ((o . os) (loop op os op os))))

(define (main args)
  (match '(#\A #\A #\B) args))

% gosh ps.scm ../test/kmp-test.scm
(define (next--10 os) (loop--2 os os))

(define (next--14 os) (case os (() #f) ((o . os) (loop--2 os os))))

(define (loop--3 ss os) (case ss (() #f) ((s . ss) (if (equal? #\A s) (case ss (() #f) ((s . ss) (if (equal? #\B s) #t (loop--2 os os)))) (next--10 os)))))

(define (main args) (case args (() #f) ((s . ss) (if (equal? #\A s) (case ss (() #f) ((s . ss) (if (equal? #\A s) (case ss (() #f) ((s . ss) (if (equal? #\B s) #t (loop--3 args args)))) (next--10 args)))) (next--14 args)))))

(define (loop--2 ss os) (case ss (() #f) ((s . ss) (if (equal? #\A s) (case ss (() #f) ((s . ss) (if (equal? #\A s) (case ss (() #f) ((s . ss) (if (equal? #\B s) #t (case os (() #f) ((o . os) (loop--2 os os)))))) (next--14 os)))) (next--14 os)))))



% cat ../test/check-tracing-confliction.scm
(define (main args)
  (case x
    [() (f u)]
    [(ca . cd) args]))

(define (f z) (g z))
(define (g args) args)

% gosh ps.scm ../test/check-tracing-confliction.scm
(define (main args) (case x (() u) ((ca . cd) args)))



% cat ../test/undefined-var-with-if.scm
(define (main args)
  (if (equal? x 1) (f x) (f x)))

(define (f zzz) zzz)

% gosh ps.scm ../test/undefined-var-with-if.scm
(define (main args) (if (equal? x 1) 1 x))



% cat ../test/undefined-var-with-case.scm
(define (main args)
  (case x
    [(ca . cd) (if (equal? ca 'A) (car x) (car x))]))

(define (car x)
  (case x
    [(ca . cd) ca]))

% gosh ps.scm ../test/undefined-var-with-case.scm
(define (main args) (case x ((ca . cd) (if (equal? ca 'A) 'A ca))))



% cat ../test/power1.scm        
(define (main args)
   (power 5 4))

(define (power x n)
   (if (equal? n 0) 1
       (* x (power x (- n 1)))
       ))

% gosh ps.scm ../test/power1.scm          
(define (main args) 625)



% cat ../test/power2.scm  
(define (main args)
   (power N 4))

(define (power x n)
   (if (equal? n 0) 1
       (* x (power x (- n 1)))
       ))

% gosh ps.scm ../test/power2.scm
(define (main args) (* N (* N (* N (* N 1)))))



% cat ../test/power3.scm      
(define (main args)
   (power 5 M))

(define (power x n)
   (if (equal? n 0) 1
       (* x (power x (- n 1)))
       ))

% gosh ps.scm ../test/power3.scm
(define (main args) (if (equal? M 0) 1 (* 5 (power--1 (- M 1)))))

(define (power--1 n) (if (equal? n 0) 1 (* 5 (power--1 (- n 1)))))



% cat ../test/if-const.scm 
(define (main args)
  (if 1 2 3))

% gosh ps.scm ../test/if-const.scm
(define (main args) 2)



% cat ../test/multi-state.scm 
(define (main args)
  (print (if (equal? X 'S1) X X))
  (print (if (equal? X 'S2) X X)))

% gosh ps.scm ../test/multi-state.scm
(define (main args) (print (if (equal? X 'S1) 'S1 X)) (print (if (equal? X 'S2) 'S2 X)))



% cat ../test/incalculable-args.scm 
(define (main args)
  (if (= (length args) 2)
      (print (power (string->number (list-ref args 1)) 4))
      -1
      ))

(define (power x n)
  (if (<= n 0) 1
      (if (= n 1) x
          (* x (power x (- n 1)))
          )))

% gosh ps.scm ../test/incalculable-args.scm
(define (main args) (if (= (length args) 2) (print (let ((x (string->number (list-ref args 1)))) (* x (* x (* x x))))) -1))



% cat ../test/let1.scm 
(define (main args)
  (let ([a 100])
    (let ([a 0]
          [b (+ a 1)])
      b)))

% gosh ps.scm ../test/let1.scm 
(define (main args) 101)



% cat ../test/let2.scm        
(define (main args)
  (let ([x (+ UNDEF 1)])
    x
    ))

% gosh ps.scm ../test/let2.scm        
(define (main args) (let ((x (+ UNDEF 1))) x))

