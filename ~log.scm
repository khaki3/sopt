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

(def (main args)
  (match '(#\A #\A #\B) args))


% gosh ps.scm test/kmp.ps.scm
(def (loop--4 ss os) (case os (() #f) ((s . ss) (if (= #\B s) #t (next--6 os)))))

(def (next--9 os) (loop--2 os os))

(def (loop--14 ss os) (case ss (() #f) ((s . ss) (if (= #\B s) (loop--15 ss os) (next--12 os)))))

(def (next--6 os) (loop--7 os os))

(def (next--12 os) (case os (() #f) ((o . os) (loop--2 os os))))

(def (loop--3 ss os) (case ss (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\B s) #t (next--9 os)))) (next--9 os)))))

(def (loop--11 ss os) #t)

(def (main args) (case args (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\B s) #t (loop--3 os os)))) (loop--2 os os)))) (loop--2 os os)))))

(def (loop--2 ss os) (case ss (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\A s) (case ss (() #f) ((s . ss) (if (= #\B s) #t (next--12 os)))) (next--12 os)))) (next--12 os)))))

(def (loop--8 ss os) #t)

(def (loop--13 ss os) (case ss (() #f) ((s . ss) (if (= #\A s) (loop--14 ss os) (next--12 os)))))

(def (loop--15 ss os) #t)

(def (loop--5 ss os) #t)

(def (match--1 s) (loop--2 s s))

(def (loop--10 ss os) (case ss (() #f) ((s . ss) (if (= #\B s) (loop--11 ss os) (next--9 os)))))

(def (loop--7 ss os) (loop--3 os os))

