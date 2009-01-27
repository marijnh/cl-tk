(asdf:defsystem :declarative-tk
  :depends-on (:cl-tk)
  :components
  ((:module "declarative-tk" :components
            ((:file "package")))))
