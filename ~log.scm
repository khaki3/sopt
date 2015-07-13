% cat test/kmp.ps.scm
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

(def (main)
  (match '(#\A #\A #\B) u))


% gosh ps.scm test/kmp.ps.scm
(def (next--6 os) (case os (() #f) ((o . os) (loop--2 os os))))

(def (loop--2 os ss)
 (case ss (() #f) ((s . ss) (if (= #\A s) (loop--3 ss os) (next--6 os)))))

(def (loop--4 os ss)
 (case ss (() #f) ((s . ss) (if (= #\B s) (loop--5 ss os) (next--6 os)))))

(def (main) (match--1 u))

(def (match--1 s) (loop--2 s s))

(def (loop--3 os ss)
 (case ss (() #f) ((s . ss) (if (= #\A s) (loop--4 ss os) (next--6 os)))))

(def (loop--5 os ss) #t)

