(defpackage #:computable-reals-asd
  (:use #:cl #:asdf))

(in-package #:computable-reals-asd)

(defsystem computable-reals
  :name "computable-reals"
  :version "1.0.1"
  :author "Michael Stoll"
  :maintainer "Robert Smith <quad@symbo1ics.com>"
  :description "Computable real numbers."
  :long-description "Arbitrary-precision, re-computing real-numbers."

  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "reals")
               (:file "get-approximations")))
