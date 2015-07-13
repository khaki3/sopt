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
(def (f--6 os) (case os (() #f) ((o . os) (f--2 os os))))

(def (f--2 os ss)
 (case ss (() #f) ((s . ss) (if (= #\A s) (f--3 ss os) (f--6 os)))))

(def (f--4 os ss)
 (case ss (() #f) ((s . ss) (if (= #\B s) (f--5 ss os) (f--6 os)))))

(def (main) (f--1 u))

(def (f--1 s) (f--2 s s))

(def (f--3 os ss)
 (case ss (() #f) ((s . ss) (if (= #\A s) (f--4 ss os) (f--6 os)))))

(def (f--5 os ss) #t)

