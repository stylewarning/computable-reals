(defpackage #:computable-reals
  (:use #:common-lisp)
  (:export #:creal
           #:approx-r
           #:make-real
           #:creal-p
           #:print-r
           #:+r
           #:-r
           #:*r
           #:/r
           #:sqrt-r
           #:log2-r
           #:log-r
           #:exp-r
           #:pi-r
           #:2pi-r
           #:sin-r
           #:cos-r
           #:*print-prec*
           #:round-r
           #:*creal-tolerance*
           #:ash-r
           #:raw-approx-r
           #:floor-r
           #:ceiling-r
           #:truncate-r
           #:pi/2-r
           #:pi/4-r
           #:atan-r
           #:expt-r
           #:tan-r))