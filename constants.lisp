;;;; constants.lisp

;;;; Definition of basic constants.

(in-package #:computable-reals)

;; Unfortunately these can only be forward referenced, and are only
;; calculated in get-approximations.lisp.

(defvar +LOG2-R+ nil "log(2) as CREAL")
(defvar +PI-R+ nil "pi as CREAL")
(defvar +2PI-R+ nil "2*pi as CREAL")
(defvar +PI/2-R+ nil "pi/2 as CREAL")
(defvar +PI/4-R+ nil "pi/4 as CREAL")


