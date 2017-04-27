(defmacro def-class (class-name &rest params)
  `(progn
     (defun ,(constructor-name class-name) (&key ,@params)
       (let ((variables (make-hash-table)))
         ,@(loop for param in params
                 collect `(setf (gethash ',param variables) ,param))
         (vector variables)))

     ,@(loop for param in params
             collect `(generate-getter ,class-name ,param))))

(defmacro generate-getter (class-name param-name)
  `(defun ,(getter-name class-name param-name) (,class-name)
     (gethash ',param-name (aref ,class-name 0))))

(defun constructor-name (class-name)
  (intern
    (format nil "MAKE-~A" class-name)))

(defun getter-name (class-name param-name)
  (intern
    (format nil "~A-~A" class-name param-name)))
