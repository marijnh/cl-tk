(eval-when (compile eval load)
  (when (ignore-errors (asdf:find-system :cffi))
    (pushnew :cffi *features*)))

(asdf:defsystem :cl-tk
  :description "Minimal bridge to Tcl/Tk"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :license "BSD"
  :depends-on (#+(and (not allegro) cffi) :cffi)
  :components ((:file "package")
               (:file "base" :depends-on ("package"))
               (:file "wish" :depends-on ("base"))
               #+(and (not allegro) cffi) (:file "cffi" :depends-on ("base"))
               #+allegro (:file "acl" :depends-on ("base"))
               #+(or cffi allegro) (:file "ffi" :depends-on (#+(and (not allegro) cffi) "cffi" #+allegro "acl"))))
