(asdf:defsystem trivial-deprecate
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library implementing a simple deprecation mechanism"
  :homepage "https://shinmera.com/docs/trivial-deprecate/"
  :bug-tracker "https://shinmera.com/project/trivial-deprecate/issues"
  :source-control (:git "https://shinmera.com/project/trivial-deprecate.git")
  :serial T
  :components ((:file "package")
               (:file "trivial-deprecate")
               (:file "documentation"))
  :depends-on ())
