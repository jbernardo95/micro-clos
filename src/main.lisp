(defmacro def-class (name &rest params)
  `(progn
     (defun ,(constructor-name name) (&key ,@params)
       (vector ,@params))))

(defun constructor-name (class-name)
  (intern
    (format nil "MAKE-~A" class-name)))
