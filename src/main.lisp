(defvar class-structures (make-hash-table)) ; So that there is no class-structures not defined error

(defmacro def-class (hierarchy &rest params)
  (let ((class-name (get-class-name hierarchy))
        (class-params (get-class-params hierarchy params)))
    `(progn
       ; Define class structures hashtable
       (if (not (boundp 'class-structures))
         (defvar class-structures (make-hash-table)))

       ; Save class structure 
       (setf (gethash ',class-name class-structures) ',class-params)

       ; Define constructor
       ,`(def-constructor ,class-name ,hierarchy ,class-params)

       ; Define recognizer 
       ,`(def-recognizer ,class-name)
       
       ; Define getters 
       ,@(loop for param in class-params
               collect `(def-getter ,class-name ,param))

       ; Define setters 
       ,@(loop for param in class-params
               collect `(def-setter ,class-name ,param)))))

(defmacro def-constructor (class-name hierarchy params)
  `(defun ,(constructor-name class-name) (&key ,@params)
     (let ((variables (make-hash-table)))
       ,@(loop for param in params
               collect `(setf (gethash ',param variables) ,param))
       (vector ',(hierarchy-as-list hierarchy) variables))))

(defmacro def-getter (class-name param-name)
  `(defun ,(getter-name class-name param-name) (object)
     (if (,(recognizer-name class-name) object)
       (let ((result nil))
         (setf result (gethash ',param-name (aref object 1)))
         result)
       (print "The given argument is not an instance of the getter class"))))

(defmacro def-setter (class-name param-name)
  `(defun ,(setter-name class-name param-name) (object value)
     (if (,(recognizer-name class-name) object)
       (setf (gethash ',param-name (aref object 1)) value)
       (print "The given argument is not an instance of the setter class"))))

(defmacro def-recognizer (class-name)
  `(defun ,(recognizer-name class-name) (object)
     (if (typep object '(simple-vector 2))
       (let ((hierarchy (aref object 0))
             (class-variables (aref object 1)))
         (if (and (typep hierarchy 'list) (typep class-variables 'hash-table))
           (loop for class in hierarchy
                 do (if (eq class ',class-name)
                      (return t))))))))

(defun get-class-name (hierarchy)
  (if (typep hierarchy 'list)
    (car hierarchy)
    hierarchy))

(defun get-class-params (hierarchy params)
  (if (typep hierarchy 'list)
    (let ((class-params params))
      (loop for class in (cdr hierarchy)
            do (setf class-params (append class-params (gethash class class-structures))))
      (remove-duplicates class-params))
    params))

(defun hierarchy-as-list (hierarchy)
  (if (typep hierarchy 'list)
    hierarchy
    (list hierarchy)))

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
