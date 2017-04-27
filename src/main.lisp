(defmacro def-class (hierarchy &rest params)
  (let ((class-name (get-class-name hierarchy)))
    `(progn
       ; Define constructor
       ,`(def-constructor ,hierarchy ,params)
       
       ; Define getters 
       ,@(loop for param in params
               collect `(def-getter ,class-name ,param))
       
       ; Define recognizer 
       ,`(def-recognizer ,hierarchy))))

(defmacro def-constructor (hierarchy params)
  (let ((class-name (get-class-name hierarchy)))
    `(defun ,(constructor-name class-name) (&key ,@params)
       (let ((variables (make-hash-table)))
         ,@(loop for param in params
                 collect `(setf (gethash ',param variables) ,param))
         (vector ',(hierarchy-as-list hierarchy) variables)))))

(defmacro def-getter (class-name param-name)
  `(defun ,(getter-name class-name param-name) (object)
     (gethash ',param-name (aref object 1))))

(defmacro def-recognizer (hierarchy)
  (let ((class-name (get-class-name hierarchy)))
    `(defun ,(recognizer-name class-name) (object)
       (let ((hierarchy (aref object 0)))
         (loop for class in hierarchy
               do (if (eq class ',class-name)
                    (return t)))))))

(defun get-class-name (hierarchy)
  (if (eq (type-of hierarchy) 'CONS)
    (car hierarchy)
    hierarchy))

(defun hierarchy-as-list (hierarchy)
  (if (eq (type-of hierarchy) 'CONS)
    hierarchy
    (list hierarchy)))

(defun constructor-name (class-name)
  (intern
    (format nil "MAKE-~A" class-name)))

(defun getter-name (class-name param-name)
  (intern
    (format nil "~A-~A" class-name param-name)))

(defun recognizer-name (class-name)
  (intern
    (format nil "~A?" class-name)))
