;;;; get-approximations.lisp
;;;; Author: Robert Smith

;;;; Here we compute the constants to around 60 digits after
;;;; everything has been loaded.

(in-package #:computable-reals)

(setf +LOG2-R+ (+r (ash-r (log-r2 1/7) 1) (log-r2 1/17))
      +PI-R+ (-r (ash-r (atan-r1 1/10) 5)
                   (ash-r (atan-r1 1/515) 4)
                   (ash-r (atan-r1 1/239) 2))
      +2PI-R+ (ash-r +pi-r+ 1)
      +PI/2-R+ (ash-r +pi-r+ -1)
      +PI/4-R+ (ash-r +pi-r+ -2))

(get-approx +log2-r+ 200)

(get-approx +2pi-r+ 200)
(get-approx +pi-r+ 200)
(get-approx +pi/2-r+ 200)
(get-approx +pi/4-r+ 200)
