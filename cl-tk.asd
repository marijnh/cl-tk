(asdf:defsystem :cl-tk
  :depends-on ()
  :components
  ((:module "cl-tk" :components
            ((:file "package")
             (:file "misc" :depends-on ("package"))
             (:file "process" :depends-on ("misc"))
             (:file "communicate" :depends-on ("process"))
             (:file "widget" :depends-on ("communicate"))))))
