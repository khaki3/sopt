% cat ../test/kmp-test.scm
(def (match p s) (loop p s p s))

(def (loop pp ss op os)
  (case pp
    (() #t)
    ((p . pp)
     (case ss
       (() #f)
       ((s . ss)
        (if (= p s)
            (loop pp ss op os)
            (next op os)
            ))))))

(def (next op os)
  (case os
    (() #f)
    ((o . os) (loop op os op os))))

(def (main args)
  (match '(#\A #\A #\B) args))

% gosh ps.scm ../test/kmp-test.scm
(def (next--10 os) (loop--2 os os))

(def (next--14 os) (case os (() #f) ((o . os) (loop--2 os os))))

(def (loop--3 ss os) (case ss (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\B s) #t (loop--2 os os)))) (next--10 os)))))

(def (main args) (case args (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\B s) #t (loop--3 os os)))) (next--10 os)))) (next--14 os)))))

(def (loop--2 ss os) (case ss (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\B s) #t (case os (() #f) ((o . os) (loop--2 os os)))))) (next--14 os)))) (next--14 os)))))



% cat ../test/check-tracing-confliction.scm
(def (main args)
  (case x
    [() (f u)]
    [(ca . cd) args]))

(def (f z) (g z))
(def (g args) args)

% gosh ps.scm ../test/check-tracing-confliction.scm
(def (main args) (case x (() u) ((ca . cd) args)))



% cat ../test/undefined-var-with-if.scm
(def (main args)
  (if (= x 1) (f x) (f x)))

(def (f zzz) zzz)

% gosh ps.scm ../test/undefined-var-with-if.scm
(def (main args) (if (= x 1) 1 x))



% cat ../test/undefined-var-with-case.scm
(def (main args)
  (case x
    [(ca . cd) (if (= ca 'A) (car x) (car x))]))

(def (car x)
  (case x
    [(ca . cd) ca]))

% gosh ps.scm ../test/undefined-var-with-case.scm
(def (main args) (case x ((ca . cd) (if (= ca 'A) 'A ca))))

