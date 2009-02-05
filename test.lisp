(in-package :cl-tk)

(defmacro with-random-tk (&body body)
  `(with-tk ((if (zerop (random 2)) 'wish-tk 'ffi-tk)) ,@body))

(defun test-button ()
  (with-random-tk
    (let ((stop nil))
      (tcl "ttk::button" ".b" :text "Exit" :command (event-handler* (setf stop t)))
      (tcl "pack .b")
      (loop :while (alive-p)
            :do (doevent t)
            :do (when stop (destroy))))
    (mainloop)))

(defun test-grid ()
  (with-random-tk
    (flet ((label (n col)
             (tcl[ "ttk::label" n :text n :background col))
           (button (n)
             (tcl[ "ttk::button" n :text n)))
      (build-grid
        (((button ".left") :rowspan 3 :sticky "news")
         ((label ".top" "#f00") :sticky "news")
         ((label ".topright" "#0f0") :sticky "news"))
        (nil
         ((label ".centerright" "#00f") :columnspan 2 :sticky "news"))
        (nil
         nil
         ((button ".bottomright") :sticky "news"))))
    (tcl "grid columnconfigure . all -weight 1")
    (tcl "grid rowconfigure . all -weight 1")
    (mainloop)))

(defun test-menu ()
  (with-random-tk
    (build-menus "."
      (menu "_File"
        (menu "_Test" (menu-item "Xxx") (menu-item "Yyy"))
        (menu-item "_Load" :command (event-handler* (print "Loading") (finish-output)))
        (menu-item "E_xit" :command (event-handler #'destroy)))
      (menu "_Help"
        (menu-item "_About" :command (tcl{ "tk_messageBox" :message "This is a test."))))
    (mainloop)))