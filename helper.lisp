(in-package :cl-tk)

(defmacro build-grid (&body rows)
  (flet ((handle-row (row rownum)
           (loop :for cell :in row :for colnum :from 0
                 :when cell
                 :collect `(tcl "grid" ,(car cell) :column ,colnum :row ,rownum (list ,@(cdr cell))))))
    `(progn
       ,@(loop :for row :in rows :for rownum :from 0
               :append (handle-row row rownum)))))

(defun add-menus (window &rest menus)
  (tcl "option add *tearOff 0")
  (let ((menubar (tcl "menu" (wname-cons window "menu"))))
    (flet ((ul (string)
             (let ((found (position #\_ string)))
               (if found
                   (cons (concatenate 'string (subseq string 0 found) (subseq string (1+ found)))
                         found)
                   (cons string nil)))))
      (tcl window "configure" :menu menubar)
      (loop :for (name . items) :in menus
            :for (label . ul) := (ul name)
            :for wname := (wname-cons menubar (as-wname label))
            :do (tcl menubar "add" "cascade" :menu (tcl[ "menu" wname) :label label
                     (if ul (list :underline ul) nil))
            :do (loop :for (name . options) :in items
                      :for (label . ul) := (ul name)
                      :do (tcl wname "add" "command" :label label options
                               (if ul (list :underline ul) nil)))))))
