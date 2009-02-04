(asdf:defsystem :cl-tk
  :depends-on (:cffi)
  :components ((:file "package")
               (:file "base" :depends-on ("package"))
               (:file "cffi" :depends-on ("base"))
               (:file "wish" :depends-on ("base"))
               (:file "start" :depends-on ("wish" "cffi"))))
