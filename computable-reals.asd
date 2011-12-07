(defpackage #:computable-reals-asd
  (:use #:cl #:asdf))

(in-package #:computable-reals-asd)

(defsystem computable-reals
  :name "computable-reals"
  :version "1.0"
  :author "Michael Stoll"
  :maintainer "Robert Smith"
  :description "Computable real numbers."
  :long-description "Arbitrary-precision, re-computing real-numbers."

  :serial t
  :components ((:file "package")
               (:file "reals")
               (:file "constants")))
