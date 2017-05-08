(load "src/load.lisp")

(def-class person name)

(def-class (teacher person) subject)

(def-class (pa-teacher teacher) class-type)
