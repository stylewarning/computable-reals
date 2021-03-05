(defsystem #:computable-reals
  :version "1.1.0"
  :author "Michael Stoll"
  :maintainer "Robert Smith <robert@stylewarning.com>"
  :description "Computable real numbers."
  :long-description "Arbitrary-precision, re-computing real-numbers."
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "reals")
               (:file "get-approximations")))
