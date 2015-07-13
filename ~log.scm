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
(def (main) (loop--2 u u))

(def (loop--9 ss os) #t)

(def
 (loop--3 ss os)
 (case
  ss
  (() #f)
  (#0=(s . ss)
   (if (= #\A s)
    (case ss (() #f) (#0# (if (= #\B s) (loop--5 ss os) (next--6 os))))
    (next--6 os)))))

(def (loop--12 ss os) #t)

(def (loop--5 ss os) #t)

(def
 (loop--2 ss os)
 (case
  ss
  (() #f)
  (#0=(s . ss)
   (if (= #\A s)
    (case ss (() #f) (#0# (if (= #\A s) (loop--4 ss os) (next--6 os))))
    (next--6 os)))))

(def (loop--15 ss os) #t)

(def
 (match--1 s)
 (case
  ss
  (() #f)
  (#0=(s . ss)
   (if (= #\A s)
    (case ss (() #f) (#0# (if (= #\A s) (case ss # #) (next--10 #))))
    (next--6 (quote (undef . undef)))))))

(def (loop--8 ss os)
 (case ss (() #f) ((s . ss) (if (= #\B s) #t (loop--2 os os)))))

(def (loop--14 ss os) (loop--7 ss (quote (#\A . undef))))

(def
 (loop--11 ss os)
 (case
  ss
  (() #f)
  ((s . ss)
   (if (= #\B s) #t (loop--14 (quote (#\A . undef)) (quote (#\A . undef)))))))

(def (next--10 os) (loop--2 os os))

(def
 (loop--4 ss os)
 (case ss (() #f)
  ((s . ss) (if (= #\B s) #t (case os (() #f) ((o . os) (loop--2 os os)))))))

(def (next--13 os) (loop--7 ss (quote (#\A . undef))))

(def (next--6 os) (case os (() #f) ((o . os) (loop--2 os os))))

(def
 (loop--7 ss os)
 (case
  ss
  (() #f)
  (#0=(s . ss)
   (if (= #\A s)
    (case ss (() #f) (#0# (if (= #\B s) (loop--9 ss #) (next--10 #))))
    (next--10 (quote (#\A . undef)))))))

