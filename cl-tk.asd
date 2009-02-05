(eval-when (compile eval load)
  (when (ignore-errors (asdf:find-system :cffi))
    (pushnew :cffi *features*)))

(asdf:defsystem :cl-tk
  :depends-on (#+cffi :cffi)
  :components ((:file "package")
               (:file "base" :depends-on ("package"))
               (:file "wish" :depends-on ("base"))
               #+cffi (:file "cffi" :depends-on ("base"))
               (:file "helper" :depends-on ("base"))))
