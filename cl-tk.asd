(asdf:defsystem :cl-tk
  :components ((:file "package")
               (:file "base" :depends-on ("package"))
               (:file "wish" :depends-on ("base"))
               (:file "helper" :depends-on ("base"))))
