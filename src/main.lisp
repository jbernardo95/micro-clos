(defmacro def-class (class-name &rest params)
  `(progn
     (defun ,(constructor-name class-name) (&key ,@params)
       (vector ,@params))
     ,@(loop for param in params
             for i from 0
             collect `(generate-getter ,class-name ,param ,i))))

(defmacro generate-getter (class-name param-name index)
  `(defun ,(getter-name class-name param-name) (,class-name)
     (aref ,class-name ,index)))

(defun constructor-name (class-name)
  (intern
    (format nil "MAKE-~A" class-name)))

(defun getter-name (class-name param-name)
  (intern
    (format nil "~A-~A" class-name param-name)))
