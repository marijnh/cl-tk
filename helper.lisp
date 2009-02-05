(in-package :cl-tk)

(defmacro build-grid (&body rows)
  (flet ((handle-row (row rownum)
           (loop :for cell :in row :for colnum :from 0
                 :when cell
                 :collect `(tcl "grid" ,(car cell) :column ,colnum :row ,rownum (list ,@(cdr cell))))))
    `(progn
       ,@(loop :for row :in rows :for rownum :from 0
               :append (handle-row row rownum)))))

(defun find-underline (string)
  (let ((found (position #\_ string)))
    (if found
        (values (concatenate 'string (subseq string 0 found) (subseq string (1+ found)))
                found)
        (values string nil))))

(defmacro build-menus (window &body menus)
  `(with-wname (wname-cons ,window "menu")
     (tcl "option add *tearOff 0")
     (tcl (wname-cdr *wname*) "configure" :menu (tcl[ "menu" *wname*))
     ,@menus))

(defmacro menu (title &body elements)
  `(flet ((body () ,@elements))
     (multiple-value-bind (title under) (find-underline ,title)
       (let ((parent *wname*))
         (with-wname (wname-cons parent (as-wname title))
           (tcl parent "add" "cascade" :menu (tcl[ "menu" *wname*) :label title
                (if under (list :underline under) nil))
           (body))))))

(defun menu-item (title &rest options)
  (multiple-value-bind (title under) (find-underline title)
    (tcl *wname* "add" "command" :label title options
         (if under (list :underline under) nil))))
