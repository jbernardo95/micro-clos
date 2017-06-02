(load "src/load.lisp")

(def-class person name)

(def-class athlete name)

(def-class (runner athlete person) field)
