;;;; realstst.lisp

;;;; Tests for Reals

(in-package #:cr)

(print-r +pi-r+ 20)
(print-r (sqrt-r 2) 20)
(print-r +pi-r+ 50)

(defvar e163 (exp-r (*r +pi-r+ (sqrt-r 163))))
(print-r e163 20)

(defvar e58 (exp-r (*r +pi-r+ (sqrt-r 58))))
(print-r e58 20)

(defun get-koeffs (x n &aux (y x) q r)
  (dotimes (i n)
    (multiple-value-setq (q r) (round-r y))
    (print q)
    (setq y (*r x r))))

(get-koeffs e163 10)
(get-koeffs e58 10)
(print-r (sin-r +pi-r+) 20)
(print-r (cos-r +pi-r+) 20)
(print-r (sin-r +pi/2-r+) 20)

