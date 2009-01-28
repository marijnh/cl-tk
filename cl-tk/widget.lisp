(in-package :cl-tk)

(defmacro build-grid (&body rows)
  (flet ((handle-row (row rownum)
           (loop :for cell :in row :for colnum :from 0
                 :when cell :collect `(tk "grid" ,(car cell)
                                          :column ,colnum :row ,rownum
                                          ,@(cdr cell)))))
    (cons 'progn (loop :for row :in rows :for rownum :from 0
                       :append (handle-row row rownum)))))
