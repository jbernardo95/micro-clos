(defmacro def-class (hierarchy &rest params)
  (let ((class-name (get-class-name hierarchy))
        (class-params (get-class-params (hierarchy-as-list hierarchy) params)))
    `(progn
       ; Define class structure 
       (defvar ,(class-structure-variable class-name) '(,@class-params))

       ; Define constructor
       ,`(def-constructor ,hierarchy ,class-params)
       
       ; Define getters 
       ,@(loop for param in class-params
               collect `(def-getter ,class-name ,param))

       ; Define setters 
       ,@(loop for param in class-params
               collect `(def-setter ,class-name ,param))
       
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

(defmacro def-setter (class-name param-name)
  `(defun ,(setter-name class-name param-name) (object value)
     (setf (gethash ',param-name (aref object 1)) value)))

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

; TODO think about how to remove eval
(defun get-class-params (hierarchy params)
  (let ((class-params params))
    (loop for class in (cdr hierarchy)
          do (setf class-params (append class-params (eval (class-structure-variable class)))))
    (remove-duplicates class-params)))

(defun hierarchy-as-list (hierarchy)
  (if (eq (type-of hierarchy) 'CONS)
    hierarchy
    (list hierarchy)))

(defun class-structure-variable (class-name)
  (intern
    (format nil "~A-CLASS-STRUCTURE" class-name)))

(defun constructor-name (class-name)
  (intern
    (format nil "MAKE-~A" class-name)))

(defun getter-name (class-name param-name)
  (intern
    (format nil "~A-~A" class-name param-name)))

(defun setter-name (class-name param-name)
  (intern
    (format nil "SET-~A-~A" class-name param-name)))

(defun recognizer-name (class-name)
  (intern
    (format nil "~A?" class-name)))
