(defpackage #:org.shirakumo.trivial-deprecate
  (:nicknames #:trivial-deprecate)
  (:use #:cl)
  (:export
   #:deprecation-warning
   #:kind
   #:name
   #:software
   #:since-version
   #:alternatives
   #:description
   #:declaim-deprecated))
