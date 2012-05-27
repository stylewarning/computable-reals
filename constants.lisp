(in-package #:computable-reals)

(defvar +LOG2-R+ (+r (ash-r (log-r2 1/7) 1) (log-r2 1/17))
  "log(2) as CREAL")

(get-approx +log2-r+ 200)           ; compute to ca. 60 decimal digits

(defvar +PI-R+ (-r (ash-r (atan-r1 1/10) 5)
                   (ash-r (atan-r1 1/515) 4)
                   (ash-r (atan-r1 1/239) 2))
  "pi as CREAL")

(defvar +2PI-R+ (ash-r +pi-r+ 1) "2*pi as CREAL")
(defvar +PI/2-R+ (ash-r +pi-r+ -1) "pi/2 as CREAL")
(defvar +PI/4-R+ (ash-r +pi-r+ -2) "pi/4 as CREAL")

(get-approx +2pi-r+ 200)            ; compute to ca. 60 decimal digits
(get-approx +pi-r+ 200)
(get-approx +pi/2-r+ 200)
(get-approx +pi/4-r+ 200)
